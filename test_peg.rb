require './peg.rb'
include PEG


describe 'peg' do
  it 'literal' do
    Literal.new('aaa').match('aaa...').should == {text: 'aaa'}
    Literal.new('aaa').match('......').should == nil
  end

  it 'regex' do
    Regex.new('a*').match('aaa...').should == {text: 'aaa'}
    Regex.new('a+').match('......').should == nil
  end

  it 'sequence' do
    grammar = Sequence.new(Regex.new('a*'), Literal.new('bbb'))
    grammar.match('aaabbb...').should == {text: 'aaabbb',
                                          children: [{text: 'aaa'},
                                                     {text: 'bbb'}]}
    grammar.match('aaa......').should == nil
  end

  it 'or' do
    grammar = Or.new(Regex.new('a+'), Regex.new('b+'))
    grammar.match('aabb').should == {text: 'aa', children: [{text: 'aa'}]}
    grammar.match('bbb...').should == {text: 'bbb', children: [{text: 'bbb'}]}
    grammar.match('...').should == nil
  end

  it 'not' do
    Not.new(Regex.new('.')).match('').should == {text: ''}
    Not.new(Regex.new('.')).match('aa').should == nil
  end

  it 'and' do
    And.new(Literal.new('a')).match('a').should == {text: ''}
    And.new(Literal.new('a')).match('b').should == nil
  end

  it 'optional' do
    grammar = Sequence.new(Optional.new(Literal.new('a')), Literal.new('b'))
    grammar.match('ab').should == {
      text: 'ab', children: [{text: 'a', children: [{text: 'a'}]},
                             {text: 'b'}]}
    grammar.match('b').should == {
      text: 'b', children: [{text: '', children: []},
                            {text: 'b'}]}
  end

  it 'one or more' do
    grammar = OneOrMore.new(Literal.new('a'))
    grammar.match('.').should == nil
    grammar.match('a.').should == {text: 'a', children: [{text: 'a'}]}
    grammar.match('aaa.').should == {text: 'aaa', children: [{text: 'a'},
                                                             {text: 'a'},
                                                             {text: 'a'}]}
  end

  it 'zero or more' do
    grammar = ZeroOrMore.new(Literal.new('a'))
    grammar.match('.').should == {text: '', children: []}
    grammar.match('aaa.').should == {text: 'aaa', children: [{text: 'a'},
                                                             {text: 'a'},
                                                             {text: 'a'}]}
    ZeroOrMore.new(Regex.new('')).match('a').should == {text: '',
                                                        children: [{text: ''}]}
  end

  it 'named' do
    grammar = Literal.new('->').name('arrow')
    grammar.name.should == 'arrow'
    grammar.match('->').should == {name: 'arrow', text: '->'}
  end
end

describe Grammar do
  it 'literal' do
    Grammar.new("rule<-'a'").grammar.should == [Literal.new('a').name('rule')]
    Grammar.new("rule<-'b'/'c'").grammar.should == [Or.new(
      Literal.new('b'), Literal.new('c')).name('rule')]
    Grammar.new("rule_1<-'a'/rule_2\nrule_2<-'b'").grammar.should == [
      Or.new(Literal.new('a'), Reference.new('rule_2')).name('rule_1'),
      Literal.new('b').name('rule_2'),
    ]
  end

  it 'has prefixes & and !' do
    Grammar.new("rule1<-'a'/&'b' rule2<-'c'/!'d'").grammar.should == [
      Or.new(Literal.new('a'), And.new(Literal.new('b'))).name('rule1'),
      Or.new(Literal.new('c'), Not.new(Literal.new('d'))).name('rule2'),
    ]
  end

  it 'has comments' do
    Grammar.new("rule_1<-'a'/rule_2#comment\nrule_2<-'b'").grammar.should == [
      Or.new(Literal.new('a'), Reference.new('rule_2')).name('rule_1'),
      Literal.new('b').name('rule_2'),
    ]
  end

  it 'has sequences' do
    Grammar.new("rule <- 'a' !'b' rule2 rule2 <- 'hai'").grammar.should == [
      Sequence.new(Literal.new('a'),
                   Not.new(Literal.new('b')),
                   Reference.new('rule2')).name('rule'),
      Literal.new('hai').name('rule2'),
    ]
  end

  it 'has suffixes ?, * and +' do
    Grammar.new("rule <-'a'? 'b'* 'c'+").grammar.should == [
      Sequence.new(Optional.new(Literal.new('a')),
                   ZeroOrMore.new(Literal.new('b')),
                   OneOrMore.new(Literal.new('c'))).name('rule')
    ]
  end

  it 'has `.` that matches all' do
    Grammar.new('rule <- "a" !.').grammar.should == [
      Sequence.new(Literal.new('a'), Not.new(Regex.new('.'))).name('rule')
    ]
  end

  it 'has character classes like [a-z]' do
    Grammar.new("rule <- [a-z] / [']").grammar.should == [
      Or.new(Regex.new('[a-z]'), Regex.new("[']")).name('rule')
    ]
  end

  it 'has grouping with (parenthesis)' do
    Grammar.new("rule <- _ ('a' / 'b') _").grammar.should == [
      Sequence.new(Reference.new('_'),
                   Or.new(Literal.new('a'), Literal.new('b')),
                   Reference.new('_')).name('rule')
    ]
  end

  it 'raises SyntaxError on invalid grammar' do
    expect do
      Grammar.new("rule <- [a-z] %$@^")
    end.to raise_error SyntaxError, "%$@^".inspect
  end

  it 'can actually parse' do
    Grammar.new("rule <- 'ru' 'le'").parse('rule').should == {
      text: 'rule', name: 'rule', children: [{text: 'ru'}, {text: 'le'}]
    }
  end

  it 'raises SyntaxError on invalid syntax' do
    expect do
      Grammar.new("rule <- [a-z]*").parse('abc123')
    end.to raise_error SyntaxError, "123".inspect
  end
end

describe ReferenceResolver do
  it 'resolves references' do
    rule1 = Or.new(Reference.new('rule2'), Literal.new('a')).name('rule1')
    rule2 = Literal.new('b').name('rule2')
    ReferenceResolver.new([rule1, rule2]).resolve.should ==
      Or.new(Literal.new('b').name('rule2'), Literal.new('a')).name('rule1')
  end

  it 'resolves references to references' do
    rule1 = Reference.new('rule2').name('rule1')
    rule2 = Reference.new('rule3').name('rule2')
    rule3 = Literal.new('a').name('rule3')
    ReferenceResolver.new([rule1, rule2, rule3]).resolve.should ==
      Literal.new('a').name('rule3')
  end
end


require './json_example'


describe JSON do
  it 'has numbers' do
    JSON.eval('42').should == 42
  end
end
