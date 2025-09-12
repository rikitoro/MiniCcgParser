
inductive Dir where
  | Fwd -- /
  | Bwd -- \
  deriving BEq

def Dir.toString : Dir → String
  | .Fwd => "/"
  | .Bwd => "\\"

instance : ToString Dir where
  toString := Dir.toString

/-- # 統語範疇 Cat -/
inductive Cat where
  | S
  | NP
  | N
  | Fun (d : Dir) (x y : Cat) -- x / y or x \ y
  deriving BEq

-- Dir の略記
infixl:30 " /> "  => Cat.Fun Dir.Fwd
infixl:30 " \\> " => Cat.Fun Dir.Bwd

def Cat.toString : Cat → String
  | .S  => "S"
  | .NP => "NP"
  | .N  => "N"
  | .Fun d x y =>
    match x, y with
    | .Fun .., .Fun .. => "(" ++ x.toString ++ ")" ++ d.toString ++ "(" ++ y.toString ++")"
    | .Fun .., _       => "(" ++ x.toString ++ ")" ++ d.toString ++ y.toString
    | _      , .Fun .. => x.toString ++ d.toString ++ "(" ++ y.toString ++")"
    | _      , _       => x.toString ++ d.toString ++ y.toString

instance : ToString Cat where
  toString :=  Cat.toString

#eval .S /> .NP
#eval .S /> .NP \> .N
#eval .S /> (.NP \> .N)
