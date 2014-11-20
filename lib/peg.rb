module PEG
  class Value
    class << self
      alias [] new
    end

    def ==(other)
      other.is_a?(self.class) && self.uninitialize == other.uninitialize
    end
    alias_method :eql?, :==

    def inspect
      self.class.to_s + uninitialize.reverse.drop_while(&:nil?).reverse.inspect
    end
    alias_method :to_s, :inspect

    def hash
      [self.class, uninitialize].hash
    end
  end

  class Node < Value
    attr_reader :source, :range, :children, :name

    def initialize(source, range, children=nil, name=nil)
      validate_range(range)
      @source, @range, @children, @name = source, range, children.to_a, name
    end

    def validate_range(range)
      unless range.exclude_end?
        raise ArgumentError,
          "ranges with exlucded end (a..b) are not allowed: #{range}"
      end
    end

    def uninitialize
      [@source, @range, @children, @name]
    end

    def self.with_children(nodes)
      first, last = nodes.first, nodes.last
      self[first.source, first.range.begin...last.range.end, nodes]
    end

    def to_s
      @source[@range]
    end
  end

  class Matchable < Value
    def map(&block)
      self.with_matchables matchables.map { |m| block.call(m).map(&block) }
    end
  end

  class TerminalMatchable < Matchable
    def matchables
      []
    end

    def with_matchables(matchables)
      raise ArgumentError, "can't change termianls" unless matchables == []
      self
    end
  end

  class Literal < TerminalMatchable
    attr_accessor :to_s

    def initialize(string)
      @to_s = string
    end

    def uninitialize
      [@to_s]
    end

    def match(source, start=0)
      @to_s.each_char.with_index do |char, i|
        return nil unless char == source[i + start]
      end
      Node[source, start...(@to_s.length + start)]
    end
  end

  class RegularExpression < TerminalMatchable
    def initialize(string)
      @re = Regexp.new('\A' + string.to_s)
    end

    def uninitialize
      [@re]
    end

    def match(source, start=0)
      sliced_string = source[start..-1]
      @re.match(sliced_string) do |match|
        Node[source, start...(match.end(0) + start)]
      end
    end
  end

  class Dot < TerminalMatchable
    def match(source, start=0)
      raise ArgumentError, '`start` ourside `source`' if source.length < start
      return nil if source.length - start == 0
      Node[source, start...(start + 1)]
    end

    def uninitialize
      []
    end
  end

  class Range < TerminalMatchable
    def initialize(range)
      validate_range(range)
      @range = range
    end

    def validate_range(range)
      if range.exclude_end?
        raise ArgumentError,
          "ranges with excluded end (a...b) are not allowed: #{range}"
      end
    end

    def uninitialize
      [@range]
    end

    def match(source, start=0)
      @range.include?(source[start]) ? Node[source, start...(start + 1)] : nil
    end
  end

  class MatchableCollection < Matchable
    attr_accessor :matchables, :uninitialize

    def initialize(*matchables)
      @uninitialize = @matchables = matchables.to_a
    end

    def with_matchables(matchables)
      self.class[*matchables]
    end
  end

  class Sequence < MatchableCollection
    def match(source, start=0)
      Node.with_children @matchables.map { |matchable|
        node = matchable.match(source, start)
        return nil if node.nil?
        start = node.range.end
        node
      }
    end
  end

  class Either < MatchableCollection
    def match(source, start=0)
      @matchables.each do |matchable|
        node = matchable.match(source, start)
        return Node.with_children([node]) unless node.nil?
      end

      nil
    end
  end

  class Class < Either
  end

  class UnaryMatchable < Matchable
    def initialize(matchable)
      @matchable = matchable
    end

    def matchables
      [@matchable]
    end
    alias_method :uninitialize, :matchables

    def with_matchables(matchables)
      message = 'UnaryMatchable can have only 1 matchable'
      raise ArgumentError, message unless matchables.length == 1
      self.class[*matchables]
    end
  end

  class Not < UnaryMatchable
    def match(source, start=0)
      @matchable.match(source, start) ? nil : Node[source, start...start]
    end
  end

  class And < Not
    def match(source, start=0)
      Not[Not[@matchable]].match(source, start)
    end
  end

  class Repeat < UnaryMatchable
    def initialize(matchable, range)
      @matchable, @range = matchable, range
    end

    def uninitialize
      [@matchable, @range]
    end

    def with_matchables(matchables)
      message = 'UnaryMatchable can have only 1 matchable'
      raise ArgumentError, message unless matchables.length == 1
      self.class[*matchables, @range]
    end

    def self.zero_or_more(matchable)
      self[matchable, 0...Float::INFINITY]
    end

    def self.one_or_more(matchable)
      self[matchable, 1...Float::INFINITY]
    end

    def self.zero_or_once(matchable)
      self[matchable, 0...2]
    end

    def match(source, start=0)
      nodes = collect_nodes(source, start)
      if nodes.length >= @range.begin
        return Node[source, start...start] if nodes == []
        return Node.with_children(nodes)
      end
      nil
    end

    def collect_nodes(source, start)
      (1...@range.end).reduce([]) { |nodes, i|
        node = @matchable.match(source, start)
        return nodes if node.nil?
        start = node.range.end
        nodes << node
      }
    end
  end

  class Reference < TerminalMatchable
    def initialize(name, dictionary)
      @name, @dictionary = name, dictionary
    end

    def uninitialize
      [@name, @dictionary]
    end

    def match(source, start=0)
      node = @dictionary.fetch(@name).match(source, start)
      return nil if node.nil?
      Node[source, node.range, [node], @name]
    end
  end

  class Grammar < Value
    attr_accessor :rules

    def initialize(source=nil)
      @rules = {}
      return if source.nil?
      @rules = PEGLanguage.eval(source.to_str, :grammar).rules
    end

    def uninitialize
      [@rules]
    end

    def inspect
      self.class.to_s + '[...]'
    end
    alias_method :to_s, :inspect

    def [](name)
      Reference[name, rules]
    end

    def []=(name, matchable)
      matchable = Sequence[matchable].map { |m| convert_from_ruby_literals m }
      @rules[name] = matchable.matchables[0]
    end

    def convert_from_ruby_literals(matchable)
      return Reference[matchable, @rules] if matchable.is_a? Symbol
      return Literal[matchable]           if matchable.is_a? String
      return RegularExpression[matchable] if matchable.is_a? Regexp
      return Sequence[*matchable]         if matchable.is_a? Array
      return Range[matchable]             if matchable.is_a? Object::Range
      matchable
    end
  end

  class Language
    def self.grammar
      @grammar ||= Grammar.new
    end

    def self.blocks
      @blocks ||= {}
    end

    def self.rule(name, matchable=nil, &block)
      name, matchable = PEGLanguage.eval(name, :definition) if matchable.nil?
      grammar[name] = matchable
      blocks[name] = block unless block.nil?
    end

    def self.eval(source, rule_name)
      eval_node grammar[rule_name].match source
    end

    DEFAULT_PROC = proc { |node, children| children}

    def self.eval_node(node)
      block = blocks.fetch(node.name) { DEFAULT_PROC }
      node = node.children[0] unless node.name.nil?
      children = node.children.map { |child| eval_node(child) }
      instance_exec(node, children, &block)
    end
  end

  class PEGLanguage < Language
    # grammar <- spacing definition+
    rule(:grammar,
         [:spacing, Repeat.one_or_more(:definition)]) do |node, children|
      _, definitions = children

      definitions.reduce(Grammar.new) { |grammar, (name, matchable)|
        grammar[name] = matchable
        grammar
      }
    end

    # definition <- identifier left_arrow expression
    rule(:definition,
         [:identifier, :left_arrow, :expression]) do |node, children|
      identifier, _, expression = children
      [identifier, expression]
    end

    # expression <- sequence (slash sequence)*
    rule(:expression,
         [:sequence,
          Repeat.zero_or_more([:slash, :sequence])]) do |node, children|
      sequence, rest = children
      rest = rest.map { |slash, sequence| sequence }
      rest.length == 0 ? sequence : Either[sequence, *rest]
    end

    # sequence <- prefix*
    rule(:sequence, Repeat.zero_or_more(:prefix)) do |node, children|
      children.length == 1 ? children[0] : Sequence[*children]
    end

    # prefix <- (and / not)? suffix
    rule(:prefix,
         [Repeat.zero_or_once(Either[:and, :not]), :suffix]) do |node, children|
      suffix = children.fetch(1)
      prefix = node.children.fetch(0).to_s

      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    # suffix <- primary (question / star / plus)?
    rule(:suffix,
         [:primary, Repeat.zero_or_once(Either[:question,
                                               :star,
                                               :plus])]) do |node, children|
      primary = children[0]
      suffix = node.children[1].to_s.strip
      { '' => primary,
        '?' => Repeat.zero_or_once(primary),
        '*' => Repeat.zero_or_more(primary),
        '+' => Repeat.one_or_more(primary),
      }.fetch(suffix)
    end

    # primary <-
    #   identifier !left_arrow / open expression close / literal / class / dot
    rule(:primary,
         Either[[:identifier, Not[:left_arrow]],
                [:open, :expression, :close],
                :literal, :class, :dot]) do |node, children|
      children.flatten[0]
    end

    # identifier = [A-Za-z0-9_]+ spacing  # HACK simplified
    rule(:identifier, [/[A-Za-z0-9_]+/, :spacing]) do |node, children|
      node.children[0].to_s.strip.to_sym
    end

    # class <- '[' (!']' range)* ']' spacing
    rule(:class,
         ['[',
          Repeat.zero_or_more([Not[']'], :range]),
          ']',
          :spacing]) do |node, children|
      _, ranges, _, _ = children
      ranges = ranges.map { |_, range| range }
      PEG::Class[*ranges]
    end

    # range <- char '-' char / char
    rule(:range, Either[[:char, '-', :char], :char]) do |node, children|
      left, _, right = children.fetch(0)
      right.nil? ? Literal[left] : Range[left..right]
    end

    ESC = {'\n' => "\n", '\r' => "\r", '\t' => "\t", "\'" => "'", '\"' => '"',
           '\[' => '[',  '\]' => ']',  '\\\\' => '\\'}

    def self.parse_escape_sequence(source)
      ESC.fetch(source) {
        octal_string = source[1..-1]
        number = octal_string.to_i(8)
        [number].pack('C*')
      }
    end

    # char <- '\\' [nrt'"\[\]\\]
    #       / '\\' [0-2][0-7][0-7]
    #       / '\\' [0-7][0-7]?
    #       / !'\\' .
    rule(:char,
         Either[['\\', PEG::Class['n', 'r', 't', "'", '"', '[', ']', '\\']],
                ['\\', ['0'..'2', '0'..'7', '0'..'7']],
                ['\\', ['0'..'7', '0'..'7']],
                ['\\', ['0'..'7']],
                [Not['\\'], Dot.new]]) do |node, children|
      source = node.to_s
      if source[0] == '\\'
        parse_escape_sequence(source)
      else
        source
      end
    end

    # literal <- ['] (!['] char)* ['] spacing
    #          / ["] (!["] char)* ["] spacing
    rule(:literal,
         Either[["'",
                 Repeat.zero_or_more([Not["'"], :char]),
                 "'",
                 :spacing],
                ['"',
                 Repeat.zero_or_more([Not['"'], :char]),
                 '"',
                 :spacing]]) do |node, children|
      Literal[children.flatten.join]
    end

    # dot <- '.' spacing
    rule(:dot, ['.', :spacing]) { |node, children| Dot.new }

    def self.token(source)
      /(?<name>\S+) *<- "(?<value>\S+)" spacing/ =~ source
      rule(name.to_sym, [value, :spacing])
    end

    token 'and        <- "&" spacing'
    token 'not        <- "!" spacing'
    token 'slash      <- "/" spacing'
    token 'left_arrow <- "<-" spacing'
    token 'question   <- "?" spacing'
    token 'star       <- "*" spacing'
    token 'plus       <- "+" spacing'
    token 'open       <- "(" spacing'
    token 'close      <- ")" spacing'

    # spacing <- (space / comment)*
    rule(:spacing, Repeat.zero_or_more(Either[:space, :comment]))

    # comment <- '#' (!end_of_line .)* end_of_line
    rule(:comment, ['#', Repeat.zero_or_more([Not[:end_of_file], Dot.new])])

    # space <- " " / "\t" / end_of_line
    rule(:space, Either[" ", "\t", :end_of_line])

    # end_of_line <- "\r\n" / "\n" / "\r"
    rule(:end_of_line, Either["\r\n", "\n", "\r"])
  end
end
