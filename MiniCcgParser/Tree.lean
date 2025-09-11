import MiniCcgParser.Cat

/-- # 適用ルール -/
inductive Rule where
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
inductive Tree where
  | Leaf (c : Cat) (token : String)
  | Node (c : Cat) (r : Rule) (left right : Tree)

private def Tree.pre : Nat → String
  | 0 => ""
  | n + 1 => "| " ++ pre n

private def Tree.reprAux (n : Nat) : Tree → String
  | .Leaf c token =>
      pre n ++
      c.repr ++ " '" ++token ++ "'"
  | .Node c r lt rt =>
      pre n ++
      c.repr ++ "  [" ++ r.repr ++ "]\n" ++
      reprAux (n + 1) lt ++ "\n" ++
      reprAux (n + 1) rt

def Tree.repr : Tree → String := reprAux 0

instance : Repr Tree where
  reprPrec dt _ := Tree.repr dt

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
