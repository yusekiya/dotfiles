---
name: test
description: Run all tests in the project by reading docs/testing.md for instructions
disable-model-invocation: false
allowed-tools: Bash, Read, Glob
---

# Run Tests

プロジェクトの全テストを実行し、結果を報告する。

## 手順

1. `docs/testing.md` を読み、テスト実行コマンドを把握する
2. `docs/testing.md` が存在しない場合は、プロジェクト構成から実行方法を推測する:
   - `pyproject.toml` があれば `uv run pytest` または `pytest` を試す
   - `Cargo.toml` があれば `cargo test` を試す
   - `package.json` があれば `npm test` を試す
3. 記載されている全テストを実行する（言語・フレームワークが複数ある場合は全て実行する）
4. 結果をまとめて報告する（合格数・失敗数・失敗したテスト名）

## 報告フォーマット

全テスト合格の場合:
```
全 N テスト合格（Python M + Rust K）
```

失敗がある場合:
```
N テスト中 F 件失敗:
- <失敗したテスト名>: <エラー概要>
```
