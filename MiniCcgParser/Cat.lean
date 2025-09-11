
inductive Dir where
  | Fwd -- /
  | Bwd -- \

def Dir.repr : Dir → String
  | .Fwd => "/"
  | .Bwd => "\\"

/-- # 統語範疇 Cat -/
inductive Cat where
  | S
  | NP
  | N
  | Fun (d : Dir) (x y : Cat) -- x / y or x \ y

-- Dir の略記
infixl:30 " /> "  => Cat.Fun Dir.Fwd
infixl:30 " \\> " => Cat.Fun Dir.Bwd

def Cat.repr : Cat → String
  | .S  => "S"
  | .NP => "NP"
  | .N  => "N"
  | .Fun d x y =>
    match x, y with
    | .Fun .., .Fun .. => "(" ++ x.repr ++ ")" ++ d.repr ++ "(" ++ y.repr ++")"
    | .Fun .., _       => "(" ++ x.repr ++ ")" ++ d.repr ++ y.repr
    | _      , .Fun .. => x.repr ++ d.repr ++ "(" ++ y.repr ++")"
    | _      , _       => x.repr ++ d.repr ++ y.repr

instance : Repr Cat where
  reprPrec cat _ := cat.repr

#eval .S /> .NP
#eval .S /> .NP \> .N
#eval .S /> (.NP \> .N)
