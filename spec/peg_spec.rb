require_relative '../lib/peg'
include PEG


describe 'peg' do
  it 'literal' do
    Literal.new('aaa').match('aaa...').should == Node.new('aaa')
    Literal.new('aaa').match('......').should == nil
  end

  it 'regex' do
    Regex.new('a*').match('aaa...').should == Node.new('aaa')
    Regex.new('a+').match('......').should == nil
  end

  it 'sequence' do
    grammar = Sequence.new(Regex.new('a*'), Literal.new('bbb'))
    grammar.match('aaabbb...').should == Node.new('aaabbb', [Node.new('aaa'),
                                                             Node.new('bbb')])
    grammar.match('aaa......').should == nil
  end

  it 'or' do
    grammar = Or.new(Regex.new('a+'), Regex.new('b+'))
    grammar.match('aabb').should == Node.new('aa', [Node.new('aa')])
    grammar.match('bbb...').should == Node.new('bbb', [Node.new('bbb')])
    grammar.match('...').should == nil
  end

  it 'not' do
    Not.new(Regex.new('.')).match('').should == Node.new('')
    Not.new(Regex.new('.')).match('aa').should == nil
  end

  it 'and' do
    And.new(Literal.new('a')).match('a').should == Node.new('')
    And.new(Literal.new('a')).match('b').should == nil
  end

  it 'optional' do
    grammar = Sequence.new(Optional.new(Literal.new('a')), Literal.new('b'))
    grammar.match('ab').should == Node.new('ab',
      [Node.new('a', [Node.new('a')]), Node.new('b')])
    grammar.match('b').should == Node.new('b', [Node.new(''), Node.new('b')])
  end

  it 'one or more' do
    grammar = OneOrMore.new(Literal.new('a'))
    grammar.match('.').should == nil
    grammar.match('a.').should == Node.new('a', [Node.new('a')])
    grammar.match('aaa.').should == Node.new('aaa', [Node.new('a')] * 3)
  end

  it 'zero or more' do
    grammar = ZeroOrMore.new(Literal.new('a'))
    grammar.match('.').should == Node.new('')
    grammar.match('aaa.').should == Node.new('aaa', [Node.new('a')] * 3)
    grammar = ZeroOrMore.new(Regex.new(''))
    grammar.match('a').should == Node.new('', [Node.new('')])
  end

  it 'named' do
    grammar = Rule.new(:arrow, Literal.new('->'))
    grammar.name.should == :arrow
    grammar.match('->').should == Node.new('->', [], :arrow)
  end
end

describe Grammar do
  it 'literal' do
    Grammar.new("rule<-'a'").grammar.should ==
      [Rule.new(:rule, Literal.new('a'))]
    Grammar.new("rule<-'b'/'c'").grammar.should ==
      [Rule.new(:rule, Or.new(Literal.new('b'), Literal.new('c')))]
    Grammar.new("rule_1<-'a'/rule_2\nrule_2<-'b'").grammar.should == [
      Rule.new(:rule_1, Or.new(Literal.new('a'), :rule_2)),
      Rule.new(:rule_2, Literal.new('b')),
    ]
  end

  it 'has prefixes & and !' do
    Grammar.new("rule1<-'a'/&'b' rule2<-'c'/!'d'").grammar.should == [
      Rule.new(:rule1, Or.new(Literal.new('a'), And.new(Literal.new('b')))),
      Rule.new(:rule2, Or.new(Literal.new('c'), Not.new(Literal.new('d')))),
    ]
  end

  it 'has comments' do
    Grammar.new("rule_1<-'a'/rule_2#comment\nrule_2<-'b'").grammar.should == [
      Rule.new(:rule_1, Or.new(Literal.new('a'), :rule_2)),
      Rule.new(:rule_2, Literal.new('b')),
    ]
  end

  it 'has sequences' do
    Grammar.new("rule <- 'a' !'b' rule2 rule2 <- 'hai'").grammar.should == [
      Rule.new(:rule, Sequence.new(Literal.new('a'),
                                   Not.new(Literal.new('b')), :rule2)),
      Rule.new(:rule2, Literal.new('hai')),
    ]
  end

  it 'has suffixes ?, * and +' do
    Grammar.new("rule <-'a'? 'b'* 'c'+").grammar.should == [
      Rule.new(:rule, Sequence.new(Optional.new(Literal.new('a')),
                                   ZeroOrMore.new(Literal.new('b')),
                                   OneOrMore.new(Literal.new('c')))),
    ]
  end

  it 'has `.` that matches all' do
    Grammar.new('rule <- "a" !.').grammar.should == [
      Rule.new(:rule, Sequence.new(Literal.new('a'), Not.new(Regex.new('.')))),
    ]
  end

  it 'has character classes like [a-z]' do
    Grammar.new("rule <- [a-z] / [']").grammar.should == [
      Rule.new(:rule, Or.new(Regex.new('[a-z]'), Regex.new("[']"))),
    ]
  end

  it 'has grouping with (parenthesis)' do
    Grammar.new("rule <- _ ('a' / 'b') _ \n _ <- ' '").grammar.should == [
      Rule.new(:rule, Sequence.new(:_, Or.new(Literal.new('a'),
                                              Literal.new('b')), :_)),
      Rule.new(:_, Literal.new(' ')),
    ]
  end

  it 'raises SyntaxError on invalid grammar' do
    expect do
      Grammar.new("rule <- [a-z] %$@^")
    end.to raise_error SyntaxError, "%$@^".inspect
  end

  it 'can actually parse' do
    Grammar.new("rule <- 'ru' 'le'").parse('rule').should == Node.new('rule',
      [Node.new('ru'), Node.new('le')], :rule)
  end

  it 'can actually parse multiple rules' do
    Grammar.new("rule <- 'ru' le \n le <- 'le'").parse('rule').should ==
      Node.new('rule', [Node.new('ru'), Node.new('le', [], :le)], :rule)
  end

  it 'raises SyntaxError on invalid syntax' do
    expect do
      Grammar.new("rule <- [a-z]*").parse('abc123')
    end.to raise_error SyntaxError, "123".inspect
  end
end

describe ReferenceResolver do
  it 'resolves references' do
    rule1 = Rule.new(:rule1, Or.new(:rule2, Literal.new('a')))
    rule2 = Rule.new(:rule2, Literal.new('b'))
    ReferenceResolver.new([rule1, rule2]).resolve.should ==
      Rule.new(:rule1,
               Or.new(Rule.new(:rule2, Literal.new('b')), Literal.new('a')))
  end

  it 'resolves references to references' do
    rule1 = Rule.new(:rule1, :rule2)
    rule2 = Rule.new(:rule2, :rule3)
    rule3 = Rule.new(:rule3, Literal.new('a'))
    ReferenceResolver.new([rule1, rule2, rule3]).resolve.should ==
      Rule.new(:rule1, Rule.new(:rule2, Rule.new(:rule3, Literal.new('a'))))
  end

  it 'resolves recursive references' do
    value = Rule.new(:value, Sequence.new(Literal.new('['),
                                          Optional.new(:value),
                                          Literal.new(']')))
    ReferenceResolver.new([value]).resolve
  end

  it 'resolves multiple recursive references' do
    value = Rule.new(:value, Sequence.new(Literal.new('['),
                                          Optional.new(:value),
                                          Optional.new(:value),
                                          Literal.new(']')))
    ReferenceResolver.new([value]).resolve
  end
end

describe Language do
  it 'does not evaluate children recursively if block arity is 1' do
    class Foo < Language
      rule('foo <- bar ""') { |node| 'ok' }  # TODO breaks with 'foo <- bar'
      rule('bar <- "foo"')  { |node| raise('fail') }
    end
    Foo.new.eval('foo').should == 'ok'
  end

  it 'allows expressions as rules' do
    class Foo < Language
      rule(Rule.new(:foo, Sequence.new(:bar, Literal.new('')))) { |_, _| 'ok' }
      foo = Rule.new(:bar, Literal.new('foo'))
      rule(foo) { |node, children| 'ok' }
    end
    Foo.new.eval('foo').should == 'ok'
  end
end
