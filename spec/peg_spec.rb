require_relative '../lib/peg'
include PEG


describe 'peg' do
  it 'literal' do
    Literal['aaa'].match('aaa...').should == Node['aaa']
    Literal['aaa'].match('......').should == nil
  end

  it 'regex' do
    Regex['a*'].match('aaa...').should == Node['aaa']
    Regex['a+'].match('......').should == nil
  end

  it 'sequence' do
    grammar = Sequence[Regex['a*'], Literal['bbb']]
    grammar.match('aaabbb...').should == Node['aaabbb',
                                              [Node['aaa'], Node['bbb']]]
    grammar.match('aaa......').should == nil
  end

  it 'or' do
    grammar = Or[Regex['a+'], Regex['b+']]
    grammar.match('aabb').should == Node['aa', [Node['aa']]]
    grammar.match('bbb...').should == Node['bbb', [Node['bbb']]]
    grammar.match('...').should == nil
  end

  it 'not' do
    Not[Regex['.']].match('').should == Node['']
    Not[Regex['.']].match('aa').should == nil
  end

  it 'and' do
    And[Literal['a']].match('a').should == Node['']
    And[Literal['a']].match('b').should == nil
  end

  it 'optional' do
    grammar = Sequence[Optional[Literal['a']], Literal['b']]
    grammar.match('ab').should == Node['ab',
                                       [Node['a', [Node['a']]], Node['b']]]
    grammar.match('b').should == Node['b', [Node[''], Node['b']]]
  end

  it 'one or more' do
    grammar = OneOrMore[Literal['a']]
    grammar.match('.').should == nil
    grammar.match('a.').should == Node['a', [Node['a']]]
    grammar.match('aaa.').should == Node['aaa', [Node['a']] * 3]
  end

  it 'zero or more' do
    grammar = ZeroOrMore[Literal['a']]
    grammar.match('.').should == Node['']
    grammar.match('aaa.').should == Node['aaa', [Node['a']] * 3]
    grammar = ZeroOrMore[Regex['']]
    grammar.match('a').should == Node['', [Node['']]]
  end

  it 'named' do
    grammar = Rule[:arrow, Literal['->']]
    grammar.name.should == :arrow
    grammar.match('->').should == Node['->', [], :arrow]
  end
end

describe Grammar do
  it 'literal' do
    Grammar.new("rule<-'a'").grammar.should == [Rule[:rule, Literal['a']]]
    Grammar.new("rule<-'b'/'c'").grammar.should == [
      Rule[:rule, Or[Literal['b'], Literal['c']]]
    ]
    Grammar.new("rule1<-'a'/rule2\nrule2<-'b'").grammar.should == [
      Rule[:rule1, Or[Literal['a'], :rule2]],
      Rule[:rule2, Literal['b']],
    ]
  end

  it 'has prefixes & and !' do
    Grammar.new("rule1<-'a'/&'b' rule2<-'c'/!'d'").grammar.should == [
      Rule[:rule1, Or[Literal['a'], And[Literal['b']]]],
      Rule[:rule2, Or[Literal['c'], Not[Literal['d']]]],
    ]
  end

  it 'has comments' do
    Grammar.new("rule1<-'a'/rule2#comment\nrule2<-'b'").grammar.should == [
      Rule[:rule1, Or[Literal['a'], :rule2]],
      Rule[:rule2, Literal['b']],
    ]
  end

  it 'has sequences' do
    Grammar.new("rule <- 'a' !'b' rule2 rule2 <- 'hai'").grammar.should == [
      Rule[:rule, Sequence[Literal['a'], Not[Literal['b']], :rule2]],
      Rule[:rule2, Literal['hai']],
    ]
  end

  it 'has suffixes ?, * and +' do
    Grammar.new("rule <-'a'? 'b'* 'c'+").grammar.should == [
      Rule[:rule, Sequence[Optional[Literal['a']],
                           ZeroOrMore[Literal['b']],
                           OneOrMore[Literal['c']]]],
    ]
  end

  it 'has `.` that matches all' do
    Grammar.new('rule <- "a" !.').grammar.should == [
      Rule[:rule, Sequence[Literal['a'], Not[Regex['.']]]],
    ]
  end

  it 'has character classes like [a-z]' do
    Grammar.new("rule <- [a-z] / [']").grammar.should == [
      Rule[:rule, Or[Regex['[a-z]'], Regex["[']"]]],
    ]
  end

  it 'has grouping with (parenthesis)' do
    Grammar.new("rule <- _ ('a' / 'b') _ \n _ <- ' '").grammar.should == [
      Rule[:rule, Sequence[:_, Or[Literal['a'], Literal['b']], :_]],
      Rule[:_, Literal[' ']],
    ]
  end

  it 'raises SyntaxError on invalid grammar' do
    expect do
      Grammar.new("rule <- [a-z] %$@^")
    end.to raise_error SyntaxError, "%$@^".inspect
  end

  it 'can actually parse' do
    Grammar.new("rule <- 'ru' 'le'").parse('rule').should ==
      Node['rule', [Node['ru'], Node['le']], :rule]
  end

  it 'can actually parse multiple rules' do
    Grammar.new("rule <- 'ru' le \n le <- 'le'").parse('rule').should ==
      Node['rule', [Node['ru'], Node['le', [], :le]], :rule]
  end

  it 'raises SyntaxError on invalid syntax' do
    expect do
      Grammar.new("rule <- [a-z]*").parse('abc123')
    end.to raise_error SyntaxError, "123".inspect
  end
end

describe ReferenceResolver do
  it 'resolves references' do
    rule1 = Rule[:rule1, Or[:rule2, Literal['a']]]
    rule2 = Rule[:rule2, Literal['b']]
    ReferenceResolver.new([rule1, rule2]).resolve.should ==
      Rule[:rule1, Or[Rule[:rule2, Literal['b']], Literal['a']]]
  end

  it 'resolves references to references' do
    rule1 = Rule[:rule1, :rule2]
    rule2 = Rule[:rule2, :rule3]
    rule3 = Rule[:rule3, Literal['a']]
    ReferenceResolver.new([rule1, rule2, rule3]).resolve.should ==
      Rule[:rule1, Rule[:rule2, Rule[:rule3, Literal['a']]]]
  end

  it 'resolves recursive references' do
    rule = Rule[:rule, Sequence[Literal['['], Optional[:rule], Literal[']']]]
    ReferenceResolver.new([rule]).resolve
  end

  it 'resolves multiple recursive references' do
    rule = Rule[:rule, Sequence[Literal['['],
                                Optional[:rule],
                                Optional[:rule],
                                Literal[']']]]
    ReferenceResolver.new([rule]).resolve
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
      rule(Rule[:foo, Sequence[:bar, Literal['']]]) { |_, _| 'ok' }
      foo = Rule[:bar, Literal['foo']]
      rule(foo) { |node, children| 'ok' }
    end
    Foo.new.eval('foo').should == 'ok'
  end
end
