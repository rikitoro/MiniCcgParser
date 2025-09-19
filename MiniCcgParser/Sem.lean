/- # Semantics -/

inductive Ty
  | e
  | t
  | arr (a b : Ty)

infixr:20 " → " => Ty.arr

#check Ty.e → Ty.t
#check Int → Int

inductive Term
  | var (x : String) (ty : Ty)
  | const (c : String) (ty : Ty)
  | lam (x : String) (ty : Ty) (t : Term)
  | app (f a : Term)
