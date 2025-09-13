import MiniCcgParser.Chart

#print Chart
#print Cell
#check Chart.lookup

def parseCCG (lex : Lexicon) (toks : List String) : List Tree :=
  let chart := Chart.fillChart lex toks
  let len := toks.length
  let trees := chart.lookup (0, len)
  trees.filter (·.cat == .S)

#eval
  parseCCG lexicon ["John"]
#eval
  parseCCG lexicon ["John", "sleeps"]
#eval
  parseCCG lexicon ["John", "likes", "Mary"]
#eval
  parseCCG lexicon ["John", "sees", "the", "dog"]
#eval
  parseCCG lexicon ["the", "dog", "likes", "John"]
