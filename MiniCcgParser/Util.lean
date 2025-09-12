def String.replicate (str : String) : Nat → String
  | 0 => ""
  | n + 1 => str ++ str.replicate n
