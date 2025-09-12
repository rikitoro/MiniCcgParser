import MiniCcgParser.Tree

/- CYK argorithm

                  [((0,3), [S [<]
                             | NP
                             | S\NP
                               | ...]),

           ((0,2),[]),          ((1,3), [S\NP [>]
                                         | (S\NP)/NP
                                         | NP]),

  ((0,1),[NP]),   ((1,2), [(S\NP)/NP]),    ((2,3),[NP])] : Chart

  0  John         1 likes                   2 Mery          3
     NP             (S\NP)/NP                 NP

  (i, j) : Span -- 合成の対象となる区間 ex. (1,3) : likes Mery の部分
  [t1, t2, ..] : List Tree -- (i, j) で指定される各セルでの可能な導出木の集まり
-/

abbrev Span := Nat × Nat

structure Cell where
  span  : Span
  trees : List Tree

abbrev Chart := List Cell

-- ch : Chart において、Span と Cell は1対1対応していることを前提とする
--      i.e. 同じ (i, j) を持つ Cell はないとする

def Chart.loopup (ch : Chart) (ij : Span): Option Cell :=
  ch.find? (·.span == ij)

def Chart.insert (ch : Chart) (ij : Span) (ts : List Tree) :=
  ({ span := ij, trees := ts } : Cell) :: ch
