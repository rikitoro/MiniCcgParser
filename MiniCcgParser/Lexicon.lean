import MiniCcgParser.Cat

abbrev Lexicon := List (String × List Cat)
-- 一つの語に対して複数のカテゴリがあり得るのでとりあえず List Cat
-- 辞書型のほうがよい？

-- 辞書
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
