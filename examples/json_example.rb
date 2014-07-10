require_relative '../lib/peg'

class JSONLanguage < PEG::Language
  rule 'value <-
          _ (string / number / object / array / bool)' do |node, children|
    children[1][0]
  end

  def collection(node, children)
    _, _, values, _ = children
    return [] if values == []
    first, rest = values[0]
    return [first] if rest == []
    [first] + rest.map {|item| item[2]}
  end

  rule 'object <- "{" _ (pair ("," _ pair)*)? "}" _' do |node, children|
    Hash[collection(node, children)]
  end

  rule 'pair <- string ":" _ value' do |node, children|
    string, _, _, value = children
    [string, value]
  end

  rule 'array <- "[" _ (value ("," _ value)*)? "]" _' do |node, children|
    collection(node, children)
  end

  rule 'bool <- ("true" / "false" / "null") _' do |node, children|
    {true: true, false: false, null: nil}[node.text.strip.to_sym]
  end

  rule 'number <- integer fraction? exponent? _' do |node, children|
    node.text.to_f
  end

  rule 'string <- ["] (!["] .)* ["] _' do |node, children|
    Kernel.eval(node.text)  # HACK
  end

  rule 'integer <- "-"? (([1-9] [0-9]+) / [0-9])'
  rule 'fraction <- "." [0-9]+'
  rule 'exponent <- [eE] [+-]? [0-9]+'
  rule '_ <- [ \t\r\n]*'  # Shortcut for optional whitespace.
end

JSON = JSONLanguage.new
