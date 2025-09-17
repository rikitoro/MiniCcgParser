def String.replicate (str : String) : Nat → String
  | 0 => ""
  | n + 1 => str ++ str.replicate n

#eval "Hoge".replicate 3
#eval "Hoge".replicate 0


def List.arange (i j : Nat) : List Nat :=
  List.range' i (j - i)

#eval List.arange 0 3
#eval List.arange 1 3
#eval List.arange 2 3
#eval List.arange 3 3
#eval List.arange 4 3


def String.toTokens (str : String) : List String :=
  str.split (· == ' ') |>.filter (·.trim ≠ "")

#eval "John likes a dog".toTokens

def List.product.{u, v} {α : Type u} {β : Type v}
  (as : List α) (bs : List β) : List (α × β) :=
  as.flatMap fun a => bs.map fun b => (a, b)

#eval [1,2,3].product ["foo", "bar"]
