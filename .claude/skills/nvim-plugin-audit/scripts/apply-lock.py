#!/usr/bin/env python3
"""Pin lazy-lock.json to the commits checked by collect-diff.py.

  apply-lock.py <out>/audited.json PLUGIN...   (or --all)

A plugin is updated only if its lockfile commit still equals the audited
`from` commit, so a lockfile changed after the audit is never overwritten.
Lines are edited in place to keep lazy.nvim's formatting. Afterwards
`:Lazy restore` checks out exactly these commits.
"""

import argparse
import json
import re
import sys
from pathlib import Path


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("audited", type=Path)
    ap.add_argument("plugins", nargs="*")
    ap.add_argument("--all", action="store_true", help="every audited plugin")
    args = ap.parse_args()

    audited = json.loads(args.audited.read_text())
    lockfile = Path(audited["lockfile"])
    names = list(audited["plugins"]) if args.all else args.plugins
    if not names:
        sys.exit("name the plugins to pin, or pass --all")
    unknown = [n for n in names if n not in audited["plugins"]]
    if unknown:
        sys.exit(f"not audited: {', '.join(unknown)}")

    text = lockfile.read_text()
    lock = json.loads(text)
    lines = text.splitlines(keepends=True)
    failed = []
    for name in names:
        a = audited["plugins"][name]
        cur = lock.get(name, {}).get("commit")
        if cur != a["from"]:
            failed.append(f"{name}: lockfile has {cur and cur[:8]}, audit started from {a['from'][:8]}; skipped")
            continue
        rx = re.compile(r'^(\s*' + re.escape(json.dumps(name)) + r'\s*:\s*\{.*"commit"\s*:\s*")'
                        + re.escape(cur) + r'(".*)$', re.S)
        hits = [i for i, l in enumerate(lines) if rx.match(l)]
        if len(hits) != 1:
            failed.append(f"{name}: could not locate a single lockfile line; skipped")
            continue
        i = hits[0]
        lines[i] = rx.sub(lambda m: m.group(1) + a["to"] + m.group(2), lines[i])
        lock[name]["commit"] = a["to"]
        print(f"{name}: {cur[:8]} -> {a['to'][:8]}")

    new = "".join(lines)
    if json.loads(new) != lock:
        sys.exit("internal error: edited lockfile does not match the expected content; nothing written")
    lockfile.write_text(new)
    for f in failed:
        print(f, file=sys.stderr)
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
