import MiniCcgParser.Tree
import MiniCcgParser.Lexicon
import MiniCcgParser.ApplyRule

/- CYK argorithm

filledChart : Chart := [

                  ⟪[0,3) : [S [<]                                <== n = 3
                            | NP
                            | S\NP
                              | ...]⟫,

          ⟪[0,2) : []⟫,          ⟪[1,3) : [S\NP [>]             <== n = 2
                                            | (S\NP)/NP
                                            | NP]⟫,

  ⟪[0,1) : [NP]⟫,   ⟪[1,2) : [(S\NP)/NP]⟫,    ⟪[2,3) : [NP]⟫    <== base (n = 1)

]

  0  John         1 likes                   2 Mery          3
    NP             (S\NP)/NP                 NP

  [i, j) : Span -- 合成の対象となる区間 ex. [1,3) : likes Mery の部分
  [t1, t2, ..] : List Tree -- [i, j) で指定される各セルでの可能な導出木の集まり
  ⟪[i,j) : [t1, t2, ..]⟫ : Cell (実質は Span × List Tree)
-/

abbrev Span := Nat × Nat

def Span.toString (ij : Span) : String :=
  let ⟨i, j⟩ := ij
  "[" ++ i.repr ++ "," ++ j.repr ++ ")"

instance : ToString Span where
  toString := Span.toString


structure Cell where
  span  : Span
  trees : List Tree

def Cell.toString (cell : Cell) : String :=
  let ⟨ij, ts⟩ := cell
  "\n⟪" ++ ij.toString ++ " : " ++ts.toString ++ "⟫"

instance : ToString Cell where
  toString := Cell.toString

#eval
  ({span := (1,2), trees := [.Leaf .N "dog"]} : Cell)

abbrev Chart := List Cell

-- ch : Chart において、Span と Cell は1対1対応していることを前提とする
--      i.e. 同じ (i, j) を持つ Cell はないとする

def Chart.lookup (ch : Chart) (ij : Span): List Tree :=
  match ch.find? (·.span == ij) with
  | some e => e.trees
  | none   => []

def Chart.insert (ch : Chart) (ij : Span) (ts : List Tree) :=
  ({ span := ij, trees := ts } : Cell) :: ch

--

/-- token 列から chart の一番下の部分 base を作る -/
def Chart.mkBase (lex : Lexicon) (toks : List String)  : Chart := Id.run do
  let mut ch : Chart := []
  let mut i : Nat := 0
  -- 左から token を見ていって cell を作って chart に追加 (末尾再帰で書けそう)
  for tok in toks do
    let cs : List Cat := lex.lookup tok
    let ts : List Tree := cs.map (.Leaf · tok)
    ch := ch.insert (i, i + 1) ts
    i  := i + 1
  return ch

#eval
  Chart.mkBase lexicon ["likes"]
#eval
  Chart.mkBase lexicon ["John", "likes", "the", "dog"] |>.reverse


--

def Chart.fillChart (lex : Lexicon) (toks : List String) : Chart := Id.run do
  let len := toks.length

  let mut ch := mkBase lex toks

  if len ≤ 1 then return ch  -- 早期リターン
  -- 以下 2 ≤ len の場合 : n = 2, 3, ..., len まで回す
  for n in List.arange 2 (len + 1) do
    ch := fillChartAt ch n
  return ch

  where
    -- span = [i, i+n) のところの cell を埋めていく
    fillChartAt (ch : Chart) (n : Nat) : Chart := Id.run do
      let mut ch' := ch
      -- 長さ n の span をすべて作る [0, n), [1, 1+n), .. [len-n, len)
      let len := toks.length
      let is : List Nat := List.arange 0 (len - n + 1)
      let spans : List Span := is.map (fun i => (i, i + n))
      for span in spans do
        let ⟨i, j⟩ := span
        -- span 左右二つに分割して、導出木を applyRules で結合
        let mut ts : List Tree := []
        for k in List.arange (i + 1) j do
          let ls : Span := (i, k)
          let rs : Span := (k, j)
          let lts : List Tree := ch.lookup ls
          let rts : List Tree := ch.lookup rs

          for lt in lts do
            for rt in rts do
              ts := applyRules lt rt ++ ts

        ch' := ch'.insert span ts
      return ch'


#eval Chart.fillChart lexicon ["John"]
#eval
  Chart.fillChart lexicon ["John", "sleeps"] |>.reverse
#eval
  Chart.fillChart lexicon ["John", "likes", "Mary"] |>.reverse
#eval
  Chart.fillChart lexicon ["John", "sees", "the", "dog"] |>.reverse
#eval
  Chart.fillChart lexicon ["the", "dog", "likes", "John"] |>.reverse

-- #eval
--   Chart.fillChart lexicon ["John", "and", "the", "dog", "likes", "Mary"] |>.reverse

-- #eval
--   Chart.fillChart lexicon ["John", "sleeps", "and", "Mary", "sees", "the", "cat"] |>.reverse
