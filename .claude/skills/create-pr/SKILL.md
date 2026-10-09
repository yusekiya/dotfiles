---
name: create-pr
description: Create a GitHub pull request with a standardized format
disable-model-invocation: false
allowed-tools: Bash, Read, Grep
---

# Create Pull Request

標準フォーマットで GitHub Pull Request を作成する。

## 手順

1. `git status` で未コミットの変更がないことを確認する
2. `git log --oneline main..HEAD` で PR に含まれるコミットを確認する
3. `git diff main..HEAD --stat` で変更ファイルを確認する
4. リモートブランチが最新か確認し、必要なら `git push -u origin <branch>` する
5. 全コミットの内容を分析して PR タイトルと本文を起草する
6. `gh pr create` で PR を作成する

## PR のフォーマット

```bash
gh pr create --title "<タイトル>" --body "$(cat <<'EOF'
## Summary
<箇条書きで変更内容を要約>

## Test plan
<テスト手順のチェックリスト>

## Post-apply actions
<変更の適用後に必要な手動操作がある場合のみ記載>

Closes #<issue番号>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

### タイトルのルール

- 70文字以内
- 詳細は body に書く

### body のルール

- `## Summary` に変更の要約を箇条書きで記述する
- `## Test plan` にテスト手順をチェックリスト形式で記述する
- `## Post-apply actions` に変更の適用後に必要な手動操作を記述する（該当する場合のみ）
- issue をクローズする場合は `Closes #<issue番号>` を含める

## 注意事項

- PR に含まれる全コミットの内容を確認してから起草する（最新コミットだけでなく全体を見る）
- push していない場合は PR 作成前に push する
