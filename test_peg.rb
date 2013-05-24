require './peg.rb'


describe 'peg' do
  it 'literal' do
    Literal.new('aaa').match('aaa...').should == 3
    Literal.new('aaa').match('......').should == nil
  end

  it 'regex' do
    Regex.new('a*').match('aaa...').should == 3
    Regex.new('a+').match('......').should == nil
  end

  it 'sequence' do
    grammar = Sequence.new(Regex.new('a*'), Literal.new('bbb'))
    grammar.match('aaabbb...').should == 6
    grammar.match('aaa......').should == nil
  end

  it 'or' do
    grammar = Or.new(Regex.new('a+'), Regex.new('b+'))
    grammar.match('aabb').should == 2
    grammar.match('bbb...').should == 3
    grammar.match('...').should == nil
  end

  it 'not' do
    Not.new(Regex.new('.')).match('').should == 0
    Not.new(Regex.new('.')).match('aa').should == nil
  end

  it 'and' do
    And.new(Literal.new('a')).match('a').should == 0
    And.new(Literal.new('a')).match('b').should == nil
  end

  it 'optional' do
    grammar = Sequence.new(Optional.new(Literal.new('a')), Literal.new('b'))
    grammar.match('ab').should == 2
    grammar.match('b').should == 1
  end

  it 'one or more' do
    grammar = OneOrMore.new(Literal.new('a'))
    grammar.match('.').should == nil
    grammar.match('a.').should == 1
    grammar.match('aaa.').should == 3
  end

  it 'zero or more' do
    grammar = ZeroOrMore.new(Literal.new('a'))
    grammar.match('.').should == 0
    grammar.match('aaa.').should == 3
  end
end
