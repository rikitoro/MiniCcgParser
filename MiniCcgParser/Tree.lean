import MiniCcgParser.Cat
import MiniCcgParser.Util

/-- # 適用ルール -/
inductive Rule : Type
  | Fa -- forward app (>)
  | Ba -- backward app (<)
  | Fc -- forward func composition (> B)
  | Bc -- backward func composition (< B)

def Rule.repr : Rule → String
  | .Fa => ">"
  | .Ba => "<"
  | .Fc => ">B"
  | .Bc => "<B"

/-- # 導出木 Tree (Derivation Tree) -/
inductive Tree : Type
  | Leaf (c : Cat) (token : String)
  | Node (c : Cat) (r : Rule) (left right : Tree)

private def Tree.toStringAux (n : Nat) : Tree → String
  | .Leaf c token =>
      pre n ++
      c.toString ++ " '" ++token ++ "'"
  | .Node c r lt rt =>
      pre n ++
      c.toString ++ "  [" ++ r.repr ++ "]\n" ++
      toStringAux (n + 1) lt ++ "\n" ++
      toStringAux (n + 1) rt
  where
    pre (n : Nat) : String := "| ".replicate n

def Tree.toString (tree : Tree) : String := "\n" ++ tree.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString

#eval Tree.Leaf .NP "John"
#eval Tree.Leaf (.S \> .NP)  "sleeps"
#eval
  Tree.Node .S .Ba
    (.Leaf .NP "John")
    (.Leaf (.S \> .NP) "Sleeps")
#eval
  Tree.Node .S .Ba
    (.Node .NP .Fa
      (.Leaf (.NP /> .N) "the")
      (.Leaf .N "dog"))
    (.Leaf (.S \> .NP) "Sleeps")

def Tree.cat : Tree → Cat
  | .Leaf cat _ => cat
  | .Node cat _ _ _ => cat
