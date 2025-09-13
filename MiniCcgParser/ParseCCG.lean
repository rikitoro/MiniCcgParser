import MiniCcgParser.Chart

#print Chart
#print Cell
#check Chart.lookup

def parseCCG (lex : Lexicon) (sentence : String) : List Tree :=
  let toks := sentence.toTokens
  let chart := Chart.fillChart lex toks
  let len := toks.length
  let trees := chart.lookup (0, len)
  trees.filter (·.cat == .S)

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
