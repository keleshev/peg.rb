require_relative '../lib/peg'

class JSONLanguage < PEG::Language
  rule 'main <- number / string' do |node, children|
    children[0]
  end

  rule 'number <- [0-9]+' do |node, children|
    node.text.to_i
  end

  rule 'string <- ["] [a-z]* ["]' do |node, children|
    Kernel.eval(node.text)
  end
end

JSON = JSONLanguage.new
