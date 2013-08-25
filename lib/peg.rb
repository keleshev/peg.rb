module PEG
  class AbstractValue
    def ==(other)
      inspect == other.inspect
    end
  end

  class Node < AbstractValue
    attr_accessor :text, :children, :name

    def initialize(text, children=[], name=nil)
      @text, @children, @name = text, children, name
    end

    def inspect
      "#{self.class}.new(#{text.inspect}, #{children.inspect}, #{name.inspect})"
    end
  end

  class AbstractRule < AbstractValue
    attr_accessor :children

    def initialize(*children)
      @children = children
    end

    def name(value=nil)
      if value
        @name = value
        self
      else
        @name
      end
    end

    def parse(source)
      node = match(source)
      if node.text.length != source.length
        raise SyntaxError.new source[node.text.length, 50].inspect
      else
        node
      end
    end

    def new_node(text, children=[])
      Node.new(text, children, @name)
    end

    def inspect
      repr = "#{self.class}.new(#{_inspect})"
      @name ? repr + ".name(#{@name.inspect})" : repr
    end
  end

  class Literal < AbstractRule
    def initialize(literal)
      @literal = literal
      @children = []
    end

    def match(text)
      text.start_with?(@literal) ? new_node(@literal) : nil
    end

    def _inspect
      @literal.inspect
    end
  end

  class Regex < Literal
    def match(text)
      res = Regexp.new('\A' + @literal).match(text)
      res && new_node(res.to_s)
    end
  end

  class Sequence < AbstractRule
    def match(text)
      text_ = String.new(text)
      len = 0
      children = []
      @children.each do |child|
        node = child.match(text_)
        return nil unless node
        children << node
        text_ = text_.slice node.text.length..text_.length
        len += node.text.length
      end
      new_node(text.slice(0...len), children)
    end

    def _inspect
      @children.map(&:inspect).join(', ')
    end
  end

  class Or < Sequence
    def match(text)
      @children.each do |child|
        node = child.match(text)
        return new_node(node.text, [node]) if node
      end
      nil
    end
  end

  class Not < Sequence
    def match(text)
      @children[0].match(text) ? nil : new_node('')
    end
  end

  class And < Sequence
    def match(text)
      @children[0].match(text) ? new_node('') : nil
    end
  end

  class OneOrMore < Sequence
    @range = (1..Float::INFINITY)

    class << self
      attr_accessor :range
    end

    def match(text)
      text_ = String.new(text)
      len = 0
      children = []
      loop do
        node = @children[0].match(text_)
        break if not node
        children << node
        break if node.text == ''
        text_ = text_.slice node.text.length..text_.length
        len += node.text.length
      end
      in_range = self.class.range.include?(children.length)
      in_range ? new_node(text.slice(0...len), children) : nil
    end
  end

  class ZeroOrMore < OneOrMore
    @range = (0..Float::INFINITY)
  end

  class Optional < OneOrMore
    @range = (0..1)
  end

  class Reference < AbstractRule
    attr_reader :reference

    def initialize(name)
      @reference = name
      @children = []
    end

    def _inspect
      @reference.inspect
    end
  end

  class Grammar < Sequence
    def initialize(source)
      #source = PEG::peg_grammar.parse(source) if source.class == String
      #@_nodes = source
      @source = source
      @children = [ReferenceResolver.new(grammar).resolve]
    end

    def match(source)
      @children[0].match(source)
    end

    def grammar
      @source.class == Array ? @source : PEGLanguage.new.eval(@source)
    end
  end

  class ReferenceResolver
    def initialize(rules)
      raise 'assertion error' if rules.class != Array
      rules = rules.map {|rule| [rule.name, rule]}
      @rules = Hash[rules]
    end

    def resolve
      name, rule = @rules.first
      _resolve(rule)
    end

    def _resolve(rule)
      if rule.class == Reference
        _resolve(@rules.fetch(rule.reference))
      else
        old_children = rule.children
        rule.children = []  # avoid infinite reqursion of _resolve
        rule.children = old_children.map {|child| _resolve(child)}
        rule
      end
    end
  end

  class Language
    class << self
      attr_accessor :rules, :blocks
    end

    def self.rule(rule, &block)
      @rules = {} if not @rules
      @blocks = {} if not @blocks
      if rule.class == String
        #rule = GrammarGenerator.visit(PEG::peg_grammar.parse(rule))[0]
        rule = PEGLanguage.new.eval(rule)[0]
      end
      name = rule.name
      @rules[name] = rule
      @blocks[name] = block
    end

    def grammar
      # we rely on the fact that 1.9+ Hash maintains order
      @grammar ||= Grammar.new(self.class.rules.values)
    end

    def eval(source)
      source = grammar.parse(source) if source.class == String
      _eval(source)
    end

    def _eval(node)
      block = self.class.blocks[node.name] || proc {|node, children| children}
      if block.arity == 2
        children = node.children.map {|child| _eval(child)}
        instance_exec(node, children, &block)
      elsif block.arity == 1
        instance_exec(node, &block)
      else
        raise "`rule` expects a block with signature |node| or |node, children|"
      end
    end
  end

  class PEGLanguage < PEG::Language
    def self.ref(name)
      Reference.new(name)
    end

    def self.lit(name)
      Literal.new(name)
    end

    # grammar <- spacing definition+
    _ = Sequence.new(ref('spacing'),
                     OneOrMore.new(ref('definition'))).name('grammar')
    rule _ do |node, children|
      spacing, definitions = children
      definitions
    end

    # definition <- identifier left_arrow expression
    _ = Sequence.new(ref('identifier'),
                     ref('left_arrow'),
                     ref('expression')).name('definition')
    rule _ do |node, children|
      identifier, left_arrow, expression = children
      expression.name(identifier.reference)
    end

    # expression <- sequence (slash sequence)*
    _ = Sequence.new(
      ref('sequence'),
      ZeroOrMore.new(Sequence.new(ref('slash'), ref('sequence')))
    ).name('expression')
    rule _ do |node, children|
      sequence, rest = children
      rest = rest.map { |slash, sequence| sequence }
      rest.length == 0 ? sequence : Or.new(sequence, *rest)
    end

    # sequence <- prefix*
    _ = ZeroOrMore.new(ref('prefix')).name('sequence')
    rule _ do |node, children|
      children.length == 1 ? children[0] : Sequence.new(*children)
    end

    # prefix <- (and / not)? suffix
    _ = Sequence.new(Optional.new(Or.new(ref('and'), ref('not'))),
                     ref('suffix')).name('prefix')
    rule _ do |node, children|
      suffix = children[1]
      prefix = node.children[0].text.strip  # HACK
      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    # suffix <- primary (question / star / plus)?
    _ = Sequence.new(
      ref('primary'),
      Optional.new(Or.new(ref('question'), ref('star'), ref('plus')))
    ).name('suffix')
    rule _ do |node, children|
      primary = children[0]
      suffix = node.children[1].text.strip  # HACK
      {
        '' => primary,
        '?' => Optional.new(primary),
        '*' => ZeroOrMore.new(primary),
        '+' => OneOrMore.new(primary),
      }.fetch(suffix)
    end

    # primary__sequence <- identifier !left_arrow
    _ = Sequence.new(ref('identifier'),
                     Not.new(ref('left_arrow'))).name('primary__sequence')
    rule _ do |node, children|
      identifier, not_left_arrow = children
      identifier
    end

    # primary__parens <- open expression close
    _ = Sequence.new(ref('open'),
                     ref('expression'),
                     ref('close')).name('primary__parens')
    rule _ do |node, children|
      open, expression, close = children
      expression
    end

    # primary <- primary__sqeuence / primary__parens / literal / class / dot
    _ = Or.new(ref('primary__sequence'),
               ref('primary__parens'),
               ref('literal'),
               ref('class'),
               ref('dot')).name('primary')
    rule _ do |node, children|
      children[0]
    end

    # identifier = [A-Za-z0-9_]+ spacing  # HACK simplified
    _ = Sequence.new(Regex.new('[A-Za-z0-9_]+'),
                     ref('spacing')).name('identifier')
    rule _ do |node, children|
      identifier_regex = node.children[0].text
      Reference.new(identifier_regex)
    end

    # class <- '[' ... ']' spacing  # HACK simplified
    _ = Sequence.new(Regex.new('\[.*?\]'), ref('spacing')).name('class')
    rule _ do |node, children|
      class_, spacing = children
      #Regex.new(class_.text)  # TODO why won't this work?
      Regex.new(node.text.strip)
    end

    # literal <- (['] ... ['] / ["] ... ["]) spacing  # HACK simplified
    _ = Sequence.new(Or.new(Regex.new("'.*?'"), Regex.new('".*?"')),
                     ref('spacing')).name('literal')
    rule _ do |node, children|
      Literal.new(Kernel.eval(node.text))
    end

    # dot <- '.' spacing
    _ = Sequence.new(lit('.'), ref('spacing')).name('dot')
    rule(_) { |node, children| Regex.new('.') }

    node = proc { |node, children| node }
    def self.token(source)
      match = /(\S+) *<- "(\S+)" spacing/.match(source)
      block = proc { |node, children| node }
      rule Sequence.new(Literal.new(match[2]),
                        Reference.new('spacing')).name(match[1]), &block
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
    _ = ZeroOrMore.new(Or.new(ref('space'), ref('comment'))).name('spacing')
    rule _, &node

    # comment <- '#' (!end_of_line .)* end_of_line
    _ = Sequence.new(
      lit('#'),
      ZeroOrMore.new(
        Sequence.new(Not.new(ref('end_of_line')), Regex.new('.'))
      ),
      ref('end_of_line')).name('comment')
    rule _, &node

    # space <- " " / "\t" / end_of_line
    rule Or.new(lit(" "), lit("\t"), ref('end_of_line')).name('space'), &node

    # end_of_line <- "\r\n" / "\n" / "\r"
    rule Or.new(lit("\r\n"), lit("\n"), lit("\r")).name('end_of_line'), &node
  end
end
