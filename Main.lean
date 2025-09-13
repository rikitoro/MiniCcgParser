import MiniCcgParser

def sample := "John runs"


def main (args : List String) : IO Unit := do
  let sentence := match args with
    | [] => sample
    | x :: _ => x
  let trees : List Tree := parseCCG lexicon sentence

  IO.print "target sentence : "
  IO.println sentence
  for t in trees do
    IO.print "---"
    IO.println t


-- Example / REPL

#eval
  parseCCG lexicon "John"
#eval
  parseCCG lexicon "John sleeps"
#eval
  parseCCG lexicon "John likes Mary"
#eval
  parseCCG lexicon "Mary likes a cat"
#eval
  parseCCG lexicon "the dog sees Mary"
