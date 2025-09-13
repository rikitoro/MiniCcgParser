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
