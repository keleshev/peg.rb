require_relative '../lib/peg'

class SimpleMath < PEG::Language
  def self.eval(source)
    super(source, :value)
  end

  rule('value  <- number / expr') { |node, children| children[0] }
  rule('number <- [0-9]+')        { |node| node.to_s.to_i }
  rule('expr   <- "(" sum ")"')   { |node, children| children[1] }

  rule('prod   <- value ("*" value)*') do |node, children|
    children.flatten.compact.inject('*')
  end

  rule('sum    <- prod ("+" prod)*') do |node, children|
    children.flatten.compact.inject('+')
  end
end

SimpleMathGrammar = PEG::Grammar.new('
  value  <- number / expr
  number <- [0-9]+
  expr   <- "(" sum ")"
  prod   <- value ("*" value)*
  sum    <- prod ("+" prod)*
')
