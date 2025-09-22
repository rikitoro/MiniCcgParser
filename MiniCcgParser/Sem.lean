/- # Semantics -/

inductive Ty.e : Type
  | mk : String → e

#check Ty.e.mk "john"

abbrev Ty.t := Prop

def Ty.IsJohn : Ty.e → Ty.t
  | .mk "john"  => True
  | _           => False

#eval
  let john := Ty.e.mk "john"
  Ty.IsJohn john
