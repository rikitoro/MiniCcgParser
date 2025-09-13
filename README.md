# MiniCcgParser

A minimalistic CCG parser implemented in Lean 4.

## 使い方

- ビルドして試す方法

プロジェクトをビルドします。

```
$ lake build
```

実行時に文を引数として与えると、解析結果が表示されます。

```
$ .lake/build/bin/miniccgparser "Mary likes the cat"
target sentence : Mary likes the cat
---
S  [<]
| NP 'Mary'
| S\NP  [>]
| | (S\NP)/NP 'likes'
| | NP  [>]
| | | NP/N 'the'
| | | N 'cat'
---
S  [<]
| NP 'Mary'
| S\NP  [>]
| | (S\NP)/N  [>B]
| | | (S\NP)/NP 'likes'
| | | NP/N 'the'
| | N 'cat'
```

- REPLで試す方法

`Main.lean` において `#eval` を使って試すこともできます。

```lean
#eval parseCCG lexicon "Mary likes a cat"
```

## ファイル構成

- `Main.lean`: メインの実行ファイル
- `MiniCcgParser/Lexicon.lean`: 語彙 `lexicon` の定義
- `MiniCcgParser/ApplyRules.lean`: 適用規則の定義
- `MiniCcgParser/Parser.lean`: CCGパーサ `parseCCG` の実装
- `MiniCcgParser/Tree.lean`: 解析木 `Tree` の定義
- `MiniCcgParser/Chart.lean`: CYKライクなチャートパーサの実装
- `MiniCcgParser/Cat.lean`: 頭語範疇 `Cat` の定義

## 今後の拡張予定(できるといいな)
- 適用規則の追加 (関数合成 (>B2), (< B2)、型繰り上げ (T) など)
- 意味合成(意味表示付きカテゴリ)