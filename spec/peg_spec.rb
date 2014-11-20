require_relative '../lib/peg'
include PEG

describe Node do
  should 'be convertible to string' do
    Node['...->...', 3...5].to_s.should == '->'
  end

  should 'be inspectable' do
    Node['a', 0...2].inspect.should == 'PEG::Node["a", 0...2, []]'
    Node['a', 0...2, [], 'hai'].inspect.should ==
      'PEG::Node["a", 0...2, [], "hai"]'
  end
end

describe Literal do
  should 'match from the beginning of string' do
    s = 'aa..'
    Literal['aa'].match(s).should == Node[s, 0...2]
  end

  should 'not match a wrong string' do
    s = '....'
    Literal['aa'].match(s).should == nil
  end

  should 'match from an arbitrary point in string' do
    s = '..aa..'
    Literal['aa'].match(s, 2).should == Node[s, 2...4]
  end
end

describe Dot do
  should 'match any single character' do
    s = 'aa..'
    Dot.new.match(s, 2).should == Node[s, 2...3]
  end

  should 'not match if there is not enough characters' do
    Dot.new.match('..', 2).should == nil
  end

  should 'raise an error if `start` is way outside the range' do
    should.raise(ArgumentError) do
      Dot.new.match('..', 9)
    end
  end
end

describe PEG::Range do
  should 'match character in range' do
    s = '..b..'
    PEG::Range['a'..'z'].match(s, 2).should == Node[s, 2...3]
  end

  should 'not match character out of range' do
    s = '..z..'
    PEG::Range['a'..'b'].match(s, 2).should == nil
  end
end

describe Sequence do
  should 'match each member sequentially' do
    s = '...aaabbb...'
    Sequence[Literal['aaa'], Literal['bbb']].match(s, 3).should ==
      Node[s, 3...9, [Node[s, 3...6], Node[s, 6...9]]]
  end

  should 'not match if out of sequence' do
    s = '...bbbaaa...'
    Sequence[Literal['aaa'], Literal['bbb']].match(s, 3).should == nil
  end
end

describe Either do
  should 'match first of its memebers if possible' do
    s = '...aaa...'
    Either[Literal['aaa'], Literal['bbb']].match(s, 3).should ==
      Node[s, 3...6, [Node[s, 3...6]]]
  end

  should 'match second member otherwise' do
    s = '...bbb...'
    Either[Literal['aaa'], Literal['bbb']].match(s, 3).should ==
      Node[s, 3...6, [Node[s, 3...6]]]
  end

  should 'return nil if none of the members matched' do
    s = '...zzz...'
    Either[Literal['aaa'], Literal['bbb']].match(s, 3).should == nil
  end
end

describe Not do
  should 'match if its member does not' do
    s = '...aaa...'
    Not[Literal['zzz']].match(s, 3).should == Node[s, 3...3]
  end

  should 'not match if its member does' do
    s = '...aaa...'
    Not[Literal['aaa']].match(s, 3).should == nil
  end
end

describe And do
  should 'match empty string if its memeber matches anything' do
    s = '...aaa...'
    And[Literal['aaa']].match(s, 3).should == Node[s, 3...3]
  end

  should 'return nil if member does not match' do
    s = '...aaa...'
    And[Literal['xxx']].match(s, 3).should == nil
  end
end

describe Repeat do
  should 'match zero times if allowed' do
    s = '...aaa...'
    Repeat[Literal['x'], 0...2].match(s, 3).should == Node[s, 3...3]
  end

  should 'match one time if allowed' do
    s = '...aaa...'
    Repeat[Literal['a'], 0...2].match(s, 3).should ==
      Node[s, 3...4, [Node[s, 3...4]]]
  end

  should 'match two times if allowed' do
    s = '...aaa...'
    Repeat[Literal['a'], 0...3].match(s, 3).should ==
      Node[s, 3...5, [Node[s, 3...4], Node[s, 4...5]]]
  end

  should 'match many times if allowed' do
    s = '...aaa...'
    Repeat[Literal['a'], 0...10].match(s, 3).should ==
      Node[s, 3...6, [Node[s, 3...4], Node[s, 4...5], Node[s, 5...6]]]
  end

  should 'not match zero times if not allowed' do
    s = '...aaa...'
    Repeat[Literal['x'], 1...10].match(s, 3).should == nil
  end

  should 'not match 3 times if not allowed' do
    s = '...aaa...'
    Repeat[Literal['a'], 5...10].match(s, 3).should == nil
  end
end

describe Reference do
  should 'match like the referenced matchable' do
    s = '...->...'
    dictionary = {arrow: Literal['->']}
    Reference[:arrow, dictionary].match(s, 3).should ==
      Node[s, 3...5, [Node[s, 3...5]], :arrow]
  end

  should 'preserve reference names in nodes in case of nested references' do
    s = '...->...'
    arrow = Literal['->']
    dictionary = {arrow: arrow}
    reference_to_arrow = Reference[:arrow, dictionary]
    dictionary[:reference_to_arrow] = reference_to_arrow
    Reference[:reference_to_arrow, dictionary].match(s, 3).should ==
      Node[s, 3...5,
           [Node[s, 3...5,
                 [Node[s, 3...5]],
                 :arrow]],
           :reference_to_arrow]
  end
end

describe Matchable do
  should 'be able to map over itself' do
    sequence = Sequence["a", Not["b"]].map do |s|
      (s.is_a? String) ? Literal[s] : s
    end
    sequence.should == Sequence[Literal["a"], Not[Literal["b"]]]
  end
end

describe PEGLanguage do
  peg = PEGLanguage

  should 'parse dot' do
    peg.eval('.', :dot).should == Dot.new
  end

  should 'parse literal' do
    peg.eval('"a"', :literal).should == Literal['a']
  end

  should 'parse literal_' do
    peg.eval('"ab\"cd\000xy\n"', :literal).should ==
      Literal['ab"cd' + "\x0xy\n"]
  end

  should 'parse a range of a single character' do
    peg.eval('a', :range).should == Literal['a']
  end

  should 'parse a real range' do
    peg.eval('a-z', :range).should == PEG::Range['a'..'z']
  end

  should 'parse a character' do
    peg.eval('a', :char).should == 'a'
  end

  should 'parse an escaped character' do
    peg.eval('\n', :char).should == "\n"
  end

  should 'parse an octal escape sequence corresponding to "A"' do
    peg.eval('\101', :char).should == "A"
  end

  should 'parse an octal escape sequence corresponding to "\x00"' do
    peg.eval('\0', :char).should == "\x00"
  end


  should 'parse an empty character class' do
    peg.eval('[]', :class).should ==
      PEG::Class[]  # XXX Not sure what semantics that should have.
  end

  should 'parse a character class with single character' do
    peg.eval('["]', :class).should ==
      PEG::Class[Literal['"']]
  end

  should 'parse a character class with single range' do
    peg.eval('[a-z]', :class).should ==
      PEG::Class[PEG::Range['a'..'z']]
  end

  should 'parse a character class with mixed characters and ranges' do
    peg.eval('[%a-z?#A-Z-]', :class).should ==
      PEG::Class[Literal['%'],
                 PEG::Range['a'..'z'],
                 Literal['?'],
                 Literal['#'],
                 PEG::Range['A'..'Z'],
                 Literal['-']]
  end

  should 'parse suffix clause' do
    peg.eval('"->"+', :suffix).should ==
      Repeat.one_or_more(Literal['->'])
  end

  should 'parse prefix clause' do
    peg.eval('!"->"', :prefix).should == Not[Literal['->']]
  end

  should 'parse prefix and suffix at the same time' do
    peg.eval('&"->"?', :sequence).should ==
      And[Repeat.zero_or_once(Literal['->'])]
  end

  should 'parse sequences' do
    peg.eval('&"->" "a" .', :sequence).should ==
      Sequence[And[Literal['->']], Literal['a'], Dot.new]
  end

  should 'parse complex sequences' do
    peg.eval('"->"? &"->" .', :sequence).should ==
      Sequence[Repeat.zero_or_once(Literal['->']),
               And[Literal['->']],
               Dot.new]
  end

  should 'parse expressions' do
    peg.eval('. / "a" "b" / "c"', :expression).should ==
      Either[Dot.new, Sequence[Literal["a"], Literal["b"]], Literal["c"]]
  end

  should 'parse definitions' do
    peg.eval('a <- "a"', :definition).should == [:a, Literal["a"]]
  end
end

describe Grammar do
  it 'can be constructed from text or code' do
    grammar = Grammar.new
    grammar[:x] = Sequence[:a, :b]
    grammar[:a] = Literal["a"]
    grammar[:b] = Literal["b"]

    source = 'x <- a b
              a <- "a"
              b <- "b"'

    grammar.should == Grammar.new(source)
  end

  it 'could be constructed from text and code at the same time' do
    grammar = Grammar.new('x <- a b').tap do |g|
      g[:a] = Literal["a"]
      g[:b] = Literal["b"]
    end

    source = 'x <- a b
              a <- "a"
              b <- "b"'

    grammar.should == Grammar.new(source)
  end
end

describe Language do
  it 'can define grammar textually with ::rule method' do
    language = Class.new Language
    source = 'x <- a b'
    language.rule(source)
    Grammar.new(source).should == language.grammar
  end

  it 'can define grammar with code using ::rule method' do
    language = Class.new Language
    language.rule(:x, Sequence[:a, :b])
    Grammar.new('x <- a b').should == language.grammar
  end

  it 'can assign action block to each rule' do
    language = Class.new Language
    language.rule(:x, Sequence['a', 'b']) { |node, children| 'hi' }
    result = language.eval('ab', :x)
    result.should == 'hi'
  end
end
