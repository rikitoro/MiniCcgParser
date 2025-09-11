import MiniCcgParser.Tree

-- Fa : forward application (x/y) y => x
def fapp : Cat → Cat → Option Cat
  | .Fun .Fwd x y, y' => if y == y' then some x else none
  | _,             _  => none

-- Ba : backward application y (x\y) => x
def bapp : Cat → Cat → Option Cat
  | y', .Fun .Bwd x y => if y == y' then some x else none
  | _,  _             => none

-- Fc : forward composition (x/y) (y/z) => (x/z)
def fcomp : Cat → Cat → Option Cat
  | .Fun .Fwd x y, .Fun .Fwd y' z => if y == y' then some (x /> z) else none
  | _,             _              => none

-- Bc : backward composition (y\z) (x\y) => (x\z)
def bcomp : Cat → Cat → Option Cat
  | .Fun .Bwd y' z, .Fun .Bwd x y => if y == y' then some (x \> z) else none
  | _,              _             => none

#eval fapp (.S /> .NP) .NP
#eval fapp (.S /> .N) .NP
#eval bapp .NP (.S \> .NP)
#eval fcomp (.S /> .NP) (.NP /> .NP)
#eval bcomp (.S \> .NP) (.NP /> .NP)
#eval bcomp (.NP \> .S) (.NP \> .NP)

def applyRules (lt rt : Tree) : List Tree :=
  [
    match fapp lt.cat rt.cat with
    | some x  => [Tree.Node x .Fa lt rt]
    | none    => []
    ,
    match bapp lt.cat rt.cat with
    | some x  => [Tree.Node x .Ba lt rt]
    | none    => []
    ,
    match fcomp lt.cat rt.cat with
    | some x  => [Tree.Node x .Fc lt rt]
    | none    => []
    ,
    match bcomp lt.cat rt.cat with
    | some x  => [Tree.Node x .Bc lt rt]
    | none    => []
  ].flatten

#eval
  let lt := Tree.Leaf .NP "John"
  let rt := Tree.Leaf (.S \> .NP) "sleeps"
  applyRules lt rt
#eval
  let lt := Tree.Leaf .NP "John"
  let rt := Tree.Leaf ((.S \> .NP) /> .NP) "likes"
  applyRules lt rt
#eval
  let lt := Tree.Leaf ((.S \> .NP) /> .NP) "likes"
  let rt := Tree.Leaf .NP "John"
  applyRules lt rt
#eval
  let lt := Tree.Leaf .NP "John"
  let rt := Tree.Node (.S \> .NP) .Fa (.Leaf ((.S \> .NP) /> .NP) "likes") (.Leaf .NP "Mary")
  applyRules lt rt
