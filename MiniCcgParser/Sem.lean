/- # Semantics -/

inductive Ty
  | e
  | t
  | arr (a b : Ty)
  deriving BEq

infixr:20 " → " => Ty.arr

def Ty.toString : Ty → String
  | e => "e"
  | t => "t"
  | arr a b => a.toString ++ " → " ++ b.toString

instance : ToString Ty where
  toString := Ty.toString

#check Ty.e → Ty.t
#check Int → Int

inductive Term
  | var (x : String) (ty : Ty)
  | const (c : String) (ty : Ty)
  | lam (x : String) (ty : Ty) (t : Term)
  | app (f a : Term)
  deriving BEq

def Term.toString : Term → String
  | var x _ => x
  | const c _ => c
  | lam x ty t => "λ" ++x ++ ":" ++ ty.toString ++ "." ++ t.toString
  | app f a =>
    let fs :=
      match f with
      | lam .. => "(" ++ f.toString ++ ")"
      | _ => f.toString
    let as :=
      match a with
      | lam .. | app .. => "(" ++ a.toString ++ ")"
      | _ => a.toString
    fs ++ " " ++ as

instance : ToString Term where
  toString := Term.toString

#eval
  Term.var "x" Ty.e
#eval
  Term.const "hoge" Ty.e
#eval
  Term.app (.lam "x" .e <| .app (.const "hoge" (.e → .t)) (.var "x" .e)) (.var "y" .e)
