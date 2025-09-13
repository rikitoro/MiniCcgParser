import MiniCcgParser.Cat

abbrev Lexicon := List (String × List Cat)

def lexicon : Lexicon :=
  [
    ("John",    [.NP]),
    ("Mary",    [.NP]),
    ("sleeps",  [.S \> .NP]),
    ("likes",   [.S \> .NP /> .NP]),
    ("sees",    [.S \> .NP /> .NP]),
    ("the",     [.NP /> .N]),
    ("a",       [.NP /> .N]),
    ("cat",     [.N]),
    ("dog",     [.N]),
  ]

#eval lexicon
#eval lexicon[3]?

def Lexicon.lookup (lex : Lexicon) (tok : String) : List Cat :=
  match lex.find? (·.fst == tok) with
  | some e => e.snd
  | none   => []

#eval lexicon.lookup "John"
#eval lexicon.lookup "likes"
#eval lexicon.lookup "hoge"
