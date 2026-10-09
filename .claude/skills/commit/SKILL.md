---
name: commit
description: Stage changes and create a git commit following project conventions
disable-model-invocation: true
allowed-tools: Bash, Read, Grep, Glob
---

# Git Commit

変更をステージングしてコミットを作成する。プロジェクトの Git 運用規則（docs/git_conventions.md が存在すればそれを参照）に従う。

## 手順

1. `git status` で未追跡・変更ファイルを確認する
2. `git diff` でステージング済みおよび未ステージングの変更内容を確認する
3. `git log --oneline -5` で直近のコミットメッセージのスタイルを確認する
4. 変更内容を分析し、コミットメッセージを起草する
5. 関連ファイルを `git add` でステージングする（`git add -A` や `git add .` は使わない）
6. コミットを作成する

## コミットメッセージのフォーマット

```
<タイトル行>

<本文（任意）>

Co-Authored-By: <モデル名> <noreply@anthropic.com>
```

`<モデル名>` には実行時の自分のモデル名を使用すること（例: `Claude Opus 4.6 (1M context)`, `Claude Sonnet 4.6` 等）。

### タイトル行のルール

- 英語で記述する
- 動詞の原形で始める（Add / Fix / Update / Implement / Remove / Bump 等）
- issue に紐づく場合はタイトル末尾に `(#<issue番号>)` を付ける

### 本文のルール

- 日本語でも英語でもよい
- 変更の目的（why）を記述する
- 省略可能（自明な変更の場合）

### コミットの作成

HEREDOC を使って改行を含むメッセージを渡す:

```bash
git commit -m "$(cat <<'EOF'
タイトル行

本文

Co-Authored-By: <モデル名> <noreply@anthropic.com>
EOF
)"
```

## 注意事項

- `.env`、credentials、秘密鍵などを含むファイルはコミットしない
- push は行わない（ユーザーから明示的に指示された場合のみ）
- `--amend` は使わない（新しいコミットを作成する）
- `--no-verify` は使わない
