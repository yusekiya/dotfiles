#!/usr/bin/env python3
"""Collect the pending lazy.nvim plugin updates for a security review.

Modes:
  (default)               installed commit -> commit `:Lazy update` would check out
  --installed-since REV   lazy-lock.json at git REV -> installed commit
                          (review updates that were already applied)

Writes <out>/summary.md, <out>/diffs/<plugin>.{runtime,other}.diff and, in the
default mode, <out>/audited.json (input of apply-lock.py), and prints the
summary path. Everything read from plugin repositories is
untrusted data.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent

# Files that never run inside Neovim. Everything else counts as runtime.
NON_RUNTIME = re.compile(
    r"""(^|/)(doc|docs|test|tests|spec|specs|\.github|\.gitlab)/
      |(^|/)[^/]*_spec\.lua$
      |\.(md|markdown|rst|txt|adoc|png|jpe?g|gif|svg|webp)$
      |(^|/)(LICENSE|LICENCE|COPYING|AUTHORS|CHANGELOG|NEWS)[^/]*$
      |(^|/)\.(gitignore|editorconfig|luarc\.json|stylua\.toml|styluaignore|luacheckrc)$
      |(^|/)(stylua|selene|\.?typos)\.toml$
    """,
    re.X,
)
# Files run by build steps (`build = ...`, build.lua, make, cargo, luarocks, npm).
BUILD = re.compile(
    r"""(^|/)(build\.lua|build\.sh|Makefile|GNUmakefile|CMakeLists\.txt|
             Cargo\.toml|Cargo\.lock|build\.rs|package\.json|package-lock\.json|
             [^/]*\.rockspec|install\.sh|setup\.py|pyproject\.toml|go\.mod|flake\.nix)$
      |\.(mk|sh|bash|zsh|ps1|bat|cmd)$
    """,
    re.X,
)
COMMENT = {".lua": "--", ".vim": '"', ".scm": ";"}

PATTERNS = [
    ("exec", r"vim\.fn\.system(list)?\b|\bsystem(list)?\s*\(|vim\.system\b|jobstart|termopen|"
             r"job_start|io\.popen|os\.execute|\b(uv|loop)\.spawn|vim\.cmd[^\n]*['\"[]\s*!|"
             r"\bexe(cute)?\s+['\"]!|^\s*:?!|\bsilent!?\s+!"),
    ("network", r"tcp_connect|new_tcp|new_udp|\bsocket\b|\bcurl\b|\bwget\b|getaddrinfo|"
                r"\bnc\s+-|/dev/tcp"),
    ("url", r"\b(https?|ftp|wss?)://"),
    ("dynamic-code", r"\bloadstring\b|\bload\s*\(|\bdofile\b|\bloadfile\b|string\.dump|"
                     r"package\.(loadlib|cpath|path|preload)|require\s*\(?\s*['\"]ffi['\"]|"
                     r"\bffi\.|luaeval|\bexecute\s*\(|nvim_exec2?\b|vim\.base64|\bfromhex\b"),
    ("fs-write", r"io\.open\s*\([^)]*['\"][wa]|fs_open\s*\([^)]*['\"][wa]|writefile|"
                 r"os\.remove|os\.rename|fs_unlink|fs_rename|fs_rmdir|delete\s*\("),
    ("secrets", r"\.ssh\b|id_rsa|id_ed25519|\.aws\b|\.netrc|\.gnupg|\.git-credentials|"
                r"credential|keychain|security\s+find-|\btoken\b|api[_-]?key|password|"
                r"\bos\.getenv|vim\.env\.|\$HOME|vim\.loop\.os_homedir|os_homedir|"
                r"stdpath\(['\"](config|data|state)"),
    ("obfuscation", r"(\\x[0-9a-fA-F]{2}){4,}|(\\\d{2,3}){4,}|string\.char\s*\(\s*\d|"
                    r"[A-Za-z0-9+/]{120,}={0,2}|string\.reverse|:reverse\(\)"),
    ("persistence", r"(init\.lua|init\.vim|\.zshrc|\.bashrc|\.profile|LaunchAgents|crontab|"
                    r"lazy-lock\.json|vimrc)\b"),
]
PATTERNS = [(k, re.compile(p, re.I)) for k, p in PATTERNS]
HIDDEN_UNICODE = re.compile("[‪-‮⁦-⁩​-‏  ﻿­]")
LONG_LINE = 400


def git(repo, *args, check=True):
    cmd = ["git", "-c", "core.quotepath=false", "-c", "diff.external=", "-C", str(repo), *args]
    r = subprocess.run(cmd, capture_output=True, text=True, errors="replace")
    if check and r.returncode != 0:
        raise RuntimeError(f"{' '.join(cmd)}: {r.stderr.strip()}")
    return r


def lazy_targets(fetch):
    with tempfile.TemporaryDirectory() as d:
        out = Path(d) / "targets.json"
        env = dict(os.environ, NVIM_AUDIT_OUT=str(out), NVIM_AUDIT_FETCH="1" if fetch else "0")
        subprocess.run(
            ["nvim", "--headless", "-c", f"luafile {HERE / 'lazy-targets.lua'}", "-c", "qa!"],
            env=env, stdin=subprocess.DEVNULL, capture_output=True, timeout=600,
        )
        if not out.exists():
            sys.exit("nvim did not write the plugin list")
        data = json.loads(out.read_text())
    if "error" in data:
        sys.exit(f"lazy-targets.lua failed:\n{data['error']}")
    return data


def classify(path):
    if BUILD.search(path):
        return "build"
    if NON_RUNTIME.search(path):
        return "other"
    return "runtime"


LUA_LONG_BRACKET = re.compile(r"--\[=*\[|\]=*\]")


def is_comment(path, line):
    suffix = Path(path).suffix
    prefix = COMMENT.get(suffix)
    s = line.strip()
    if s == "":
        return True
    if prefix is None or not s.startswith(prefix):
        return False
    # `--[[ ]] code` and `--]] code` run the code after the bracket; treat any
    # long bracket as code so it is never hidden as "comment-only".
    return not (suffix == ".lua" and LUA_LONG_BRACKET.search(s))


def file_diffs(repo, a, b):
    """Split a full `git diff` into {path: text}."""
    text = git(repo, "diff", "--text", "--no-ext-diff", "--no-textconv", "--no-color",
               "--find-renames", a, b).stdout
    files, cur, buf = {}, None, []
    for line in text.splitlines(keepends=True):
        m = re.match(r"diff --git a/(.*) b/(.*)$", line.rstrip("\n"))
        if m:
            if cur:
                files[cur] = "".join(buf)
            cur, buf = m.group(2), []
        buf.append(line)
    if cur:
        files[cur] = "".join(buf)
    return files


def added_lines(diff):
    n = 0
    for line in diff.splitlines():
        if line.startswith("@@"):
            m = re.search(r"\+(\d+)", line)
            n = int(m.group(1)) - 1 if m else 0
        elif line.startswith("+") and not line.startswith("+++"):
            n += 1
            yield n, line[1:]
        elif not line.startswith("-"):
            n += 1


def changed_code(path, diff):
    """True if some added/removed line is not a comment or blank."""
    for line in diff.splitlines():
        if line[:1] in "+-" and line[:3] not in ("+++", "---") and not is_comment(path, line[1:]):
            return True
    return False


def scan(path, diff, cls):
    hits = []
    for n, line in added_lines(diff):
        if HIDDEN_UNICODE.search(line):
            hits.append(("hidden-unicode", n, ascii(line)))
        # Comments cannot run; only hidden characters matter there.
        if is_comment(path, line):
            continue
        for kind, rx in PATTERNS:
            if kind == "url" and cls == "other":
                continue
            if rx.search(line):
                hits.append((kind, n, line))
        if len(line) > LONG_LINE:
            hits.append(("long-line", n, line[:160] + f"... ({len(line)} chars)"))
    return hits


def audit(p, out_dir):
    repo, a, b = p["dir"], p["from"], p["to"]
    r = {"name": p["name"], "url": p.get("url"), "from": a, "to": b, "warnings": []}
    for c in (a, b):
        if git(repo, "cat-file", "-e", f"{c}^{{commit}}", check=False).returncode != 0:
            r["warnings"].append(f"commit {c} not present locally (run with --fetch)")
            return r
    if git(repo, "merge-base", "--is-ancestor", a, b, check=False).returncode != 0:
        r["warnings"].append("NOT FAST-FORWARD: target is not a descendant of the current commit "
                             "(history rewrite / force-push, or a downgrade)")
    fmt = "%h%x1f%an%x1f%ae%x1f%cn%x1f%ce%x1f%G?%x1f%ad%x1f%s"
    known = set(git(repo, "log", "--format=%ae%n%ce", a).stdout.split())
    r["commits"] = []
    for line in git(repo, "log", "--date=short", f"--format={fmt}", f"{a}..{b}").stdout.splitlines():
        h, an, ae, cn, ce, sig, date, subj = line.split("\x1f")
        new = sorted({e for e in (ae, ce) if e not in known and e != "noreply@github.com"})
        r["commits"].append(dict(h=h, an=an, ae=ae, cn=cn, ce=ce, sig=sig, date=date, subj=subj, new=new))
    r["count"] = len(r["commits"])

    raw = git(repo, "diff", "--raw", "--no-renames", "-z", a, b).stdout.split("\0")
    for meta, path in zip(raw[0::2], raw[1::2]):
        old_mode, new_mode = meta.lstrip(":").split()[:2]
        if new_mode in ("120000", "160000") or old_mode in ("120000", "160000"):
            r["warnings"].append(f"symlink/submodule change: {path} ({old_mode}->{new_mode})")
        elif new_mode == "100755" and old_mode != "100755":
            r["warnings"].append(f"file became executable: {path}")

    r["files"], r["hits"] = [], []
    diffs = {"runtime": [], "other": []}
    for path, diff in file_diffs(repo, a, b).items():
        cls = classify(path)
        if cls == "runtime" and Path(path).suffix in COMMENT and not changed_code(path, diff):
            cls = "comment-only"
        if "\nGIT binary patch" in diff or "\nBinary files " in diff:
            r["warnings"].append(f"binary file changed: {path}")
        body = [l for l in diff.splitlines() if l[:3] not in ("+++", "---")]
        ad = sum(l.startswith("+") for l in body)
        de = sum(l.startswith("-") for l in body)
        r["files"].append((cls, path, ad, de))
        r["hits"] += [(cls, path, *h) for h in scan(path, diff, cls)]
        diffs["runtime" if cls in ("runtime", "build") else "other"].append(diff)

    safe = re.sub(r"[^\w.-]", "_", p["name"])
    r["diff_files"] = {}
    for kind, parts in diffs.items():
        if parts:
            f = out_dir / "diffs" / f"{safe}.{kind}.diff"
            f.write_text("".join(parts))
            r["diff_files"][kind] = (f, sum(x.count("\n") for x in parts))
    return r


def render(results, extra, mode):
    order = {"build": 0, "runtime": 1, "comment-only": 2, "other": 3}
    o = [f"# nvim plugin audit ({mode})", ""]
    o += [f"- {w}" for w in extra]
    if extra:
        o.append("")
    if not results:
        o.append("No pending updates.")
    else:
        o += ["| plugin | from | audited to | commits |", "|---|---|---|---|"]
        o += [f"| {r['name']} | {r['from'][:8]} | {r['to'][:8]} | {r.get('count', '?')} |" for r in results]
        o.append("")
    for r in results:
        o += [f"## {r['name']}  ({r['from'][:8]}..{r['to'][:8]}, {r.get('count', '?')} commits)",
              f"repo: {r['url']}", ""]
        for w in r["warnings"]:
            o.append(f"**WARNING** {w}")
        if r["warnings"]:
            o.append("")
        if "commits" not in r:
            continue
        o.append("### commits  (sig: G=good U=untrusted N=none B=bad E=can't check)")
        for c in r["commits"]:
            who = f"{c['an']} <{c['ae']}>"
            if (c["cn"], c["ce"]) != (c["an"], c["ae"]):
                who += f" / committer {c['cn']} <{c['ce']}>"
            flag = f"  **FIRST-TIME: {', '.join(c['new'])}**" if c["new"] else ""
            o.append(f"- {c['h']} {c['date']} sig={c['sig']} {who}{flag}\n  {c['subj']}")
        o += ["", "### files  (class  +added -deleted  path)"]
        for cls, path, ad, de in sorted(r["files"], key=lambda x: (order[x[0]], x[1])):
            o.append(f"- {cls:12} +{ad} -{de}  {path}")
        o += ["", "### pattern hits on added lines"]
        if not r["hits"]:
            o.append("(none)")
        for cls, path, kind, n, line in sorted(r["hits"], key=lambda x: (order[x[0]], x[1], x[3])):
            o.append(f"- [{kind}] {path}:{n} ({cls})  `{line.strip()[:200]}`")
        o += ["", "### diff files"]
        for kind, (f, n) in r["diff_files"].items():
            o.append(f"- {kind}: {f}  ({n} lines)")
        o.append("")
    return "\n".join(o) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fetch", action="store_true", help="run `:Lazy check` (git fetch) first")
    ap.add_argument("--installed-since", metavar="REV",
                    help="audit lockfile@REV -> installed commits instead of pending updates")
    ap.add_argument("--out", help="output directory (default: new temp dir)")
    ap.add_argument("plugins", nargs="*", help="limit to these plugin names")
    args = ap.parse_args()

    out_dir = Path(args.out or tempfile.mkdtemp(prefix="nvim-plugin-audit-"))
    (out_dir / "diffs").mkdir(parents=True, exist_ok=True)
    data = lazy_targets(args.fetch and not args.installed_since)
    lockfile = Path(data["lockfile"]).resolve()
    lock = json.loads(lockfile.read_text()) if lockfile.exists() else {}
    extra = []

    if args.installed_since:
        mode = f"lazy-lock.json@{args.installed_since} -> installed"
        r = git(lockfile.parent, "show", f"{args.installed_since}:./{lockfile.name}", check=False)
        if r.returncode != 0:
            sys.exit(r.stderr.strip())
        base = json.loads(r.stdout)
    else:
        mode = "installed -> :Lazy update target"
        base = None

    todo = []
    for p in sorted(data["plugins"], key=lambda p: p["name"].lower()):
        name = p["name"]
        if args.plugins and name not in args.plugins:
            continue
        if not p["installed"]:
            extra.append(f"**NOT INSTALLED: {name}** ({p.get('url')}) — `:Lazy update` will install it; "
                         "the whole repository needs review")
            continue
        if p["is_local"] or not p.get("from"):
            continue
        if base is not None:
            if name not in base:
                extra.append(f"**NEW SINCE {args.installed_since}: {name}** — not in the old lockfile; "
                             "review the whole repository")
                continue
            p = dict(p, to=p["from"], **{"from": base[name]["commit"]})
        else:
            locked = lock.get(name, {}).get("commit")
            if locked and locked != p["from"]:
                extra.append(f"{name}: installed {p['from'][:8]} differs from lazy-lock.json {locked[:8]}")
            if not p.get("to"):
                extra.append(f"{name}: lazy.nvim could not resolve an update target")
                continue
        if p["from"] != p["to"]:
            todo.append(p)

    results = [audit(p, out_dir) for p in todo]
    summary = out_dir / "summary.md"
    summary.write_text(render(results, extra, mode))
    if base is None:
        audited = {"lockfile": str(lockfile),
                   "plugins": {r["name"]: {"from": r["from"], "to": r["to"]}
                               for r in results if "commits" in r}}
        (out_dir / "audited.json").write_text(json.dumps(audited, indent=2) + "\n")
    print(summary)


if __name__ == "__main__":
    main()
