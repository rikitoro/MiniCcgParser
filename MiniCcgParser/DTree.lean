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

/-- # 導出木 DTree (Derivation Tree) -/
inductive DTree where
  | Leaf (c : Cat) (token : String)
  | Node (c : Cat) (r : Rule) (left right : DTree)

private def DTree.pre : Nat → String
  | 0 => ""
  | n + 1 => "| " ++ pre n

private def DTree.reprAux (n : Nat) : DTree → String
  | .Leaf c token =>
      pre n ++
      c.repr ++ " '" ++token ++ "'"
  | .Node c r ldt rdt =>
      pre n ++
      c.repr ++ "  [" ++ r.repr ++ "]\n" ++
      reprAux (n + 1) ldt ++ "\n" ++
      reprAux (n + 1) rdt

def DTree.repr : DTree → String := reprAux 0

instance : Repr DTree where
  reprPrec dt _ := DTree.repr dt

#eval DTree.Leaf .NP "John"
#eval DTree.Leaf (.S \> .NP)  "sleeps"
#eval
  DTree.Node .S .Ba
    (.Leaf .NP "John")
    (.Leaf (.S \> .NP) "Sleeps")
#eval
  DTree.Node .S .Ba
    (.Node .NP .Fa
      (.Leaf (.NP /> .N) "the")
      (.Leaf .N "dog"))
    (.Leaf (.S \> .NP) "Sleeps")
