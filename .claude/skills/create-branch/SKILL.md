---
name: create-branch
description: Create a feature branch linked to a GitHub issue
disable-model-invocation: true
argument-hint: <issue-number>
allowed-tools: Bash
---

# Create Branch

GitHub issue に紐づくブランチを作成する。

## 使い方

```
/create-branch 2
```

## 手順

1. `gh issue view $ARGUMENTS` で issue のタイトルを取得する
2. タイトルからブランチ名を生成する
3. `git checkout -b <ブランチ名>` でブランチを作成する

## ブランチ名のフォーマット

```
<変更のタイプ>/#<issue番号>_<タイトル>
```

- タイトルはケバブケース（小文字、単語区切りはハイフン）で記述する
- issue のタイトルを要約して短くする（長すぎる場合は重要な部分のみ）

### 変更のタイプ

issue の内容から判断する:

| タイプ | 用途 |
|---|---|
| `feature` | 新機能の追加 |
| `fix` | バグ修正 |
| `docs` | ドキュメントのみの変更 |
| `refactor` | 機能変更を伴わないコードの改善 |
| `test` | テストの追加・修正 |

### 例

```
feature/#2_gap-filling-dispatch-for-large-jobs
fix/#15_cancel-race-condition
```
