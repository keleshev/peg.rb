require_relative '../lib/peg'

class JSONLanguage < PEG::Language
  rule 'value <- string / number / object / array / bool' do |node, children|
    children[0]
  end

  def collection(node, children)
    _, values, _ = children
    return [] if values == []
    first, rest = values[0]
    return [first] if rest == []
    [first] + rest.map {|item| item[1]}
  end

  rule 'object <- "{" (pair ("," pair)*)? "}"' do |node, children|
    Hash[collection(node, children)]
  end

  rule 'pair <- string ":" number' do |node, children| # number should be value
    string, _, value = children
    [string, value]
  end

  rule 'array <- "[" (value ("," value)*)? "]"' do |node, children|
    collection(node, children)
  end

  rule 'bool <- "true" / "false" / "null"' do |node, children|
    {true: true, false: false, null: nil}[node.text.to_sym]
  end

  rule 'number <- int frac exp / int exp / int frac / int' do |node, children|
    node.text.to_f
  end

  rule 'string <- ["] (!["] .)* ["]' do |node, children|
    Kernel.eval(node.text)
  end

  rule 'int <- "-"? (([1-9] [0-9]+) / [0-9])'
  rule 'frac <- "." [0-9]+'
  rule 'exp <- [eE] [+-]? [0-9]+'
  rule '_ <- [ \t\r\n]*'  # optional whitespace
end

JSON = JSONLanguage.new
