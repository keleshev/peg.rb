module PEG
  class AbstractValue
    class << self
      alias [] new
    end

    def ==(other)
      inspect == other.inspect
    end

    def inspect
      "#{self.class}[#{arguments.map(&:inspect).join(', ')}]"
    end
  end

  class Node < AbstractValue
    attr_accessor :text, :children, :name

    def initialize(text, children=[], name=nil)
      @text, @children, @name = text, children, name
    end

    def arguments
      [@text, @children, @name]
    end
  end

  class AbstractRule < AbstractValue
    attr_accessor :children

    def initialize(*children)
      @children = children
    end

    def parse(source)
      node = match(source)
      if node.text.length != source.length
        raise SyntaxError.new source[node.text.length, 50].inspect
      else
        node
      end
    end
  end

  class Rule < AbstractRule
    attr_accessor :name

    def initialize(literal, child)
      @name = literal
      @children = [child]
    end

    def match(text)
      node = @children[0].match(text)
      node.name = @name if node
      node
    end

    def arguments
      [@name, @children[0]]
    end
  end

  class Literal < AbstractRule
    def initialize(literal)
      @literal = literal
      @children = []
    end

    def match(text)
      text.start_with?(@literal) ? Node[@literal] : nil
    end

    def arguments
      [@literal]
    end
  end

  class Regex < Literal
    def match(text)
      res = Regexp.new('\A' + @literal).match(text)
      res && Node[res.to_s]
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
      Node[text.slice(0...len), children]
    end

    def arguments
      @children
    end
  end

  class Or < Sequence
    def match(text)
      @children.each do |child|
        node = child.match(text)
        return Node[node.text, [node]] if node
      end
      nil
    end
  end

  class Not < Sequence
    def match(text)
      @children[0].match(text) ? nil : Node['']
    end
  end

  class And < Sequence
    def match(text)
      @children[0].match(text) ? Node[''] : nil
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
      in_range ? Node[text.slice(0...len), children] : nil
    end
  end

  class ZeroOrMore < OneOrMore
    @range = (0..Float::INFINITY)
  end

  class Optional < OneOrMore
    @range = (0..1)
  end

  class Grammar < Sequence
    def initialize(source)
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
      rules = rules.map { |rule| [rule.name, rule] }
      @rules = Hash[rules]
    end

    def resolve
      name, rule = @rules.first
      _resolve(rule)
    end

    def _resolve(rule)
      if rule.class == Symbol
        _resolve(@rules.fetch(rule))
      else
        old_children = rule.children
        rule.children = []  # avoid infinite reqursion of _resolve
        rule.children = old_children.map { |child| _resolve(child) }
        rule
      end
    end
  end

  class Language
    class << self
      attr_accessor :rules, :blocks
    end

    def self.rule(rule, &block)
      @rules = [] if not @rules
      @blocks = {} if not @blocks
      rule = PEGLanguage.new.eval(rule)[0] if rule.class == String
      @rules << rule
      @blocks[rule.name] = block
    end

    def grammar
      @grammar ||= Grammar.new(self.class.rules)
    end

    def eval(source)
      source = grammar.parse(source) if source.class == String
      _eval(source)
    end

    def _eval(node)
      block = self.class.blocks[node.name] || proc { |node, children| children }
      if block.arity == 2
        children = node.children.map { |child| _eval(child) }
        instance_exec(node, children, &block)
      elsif block.arity == 1
        instance_exec(node, &block)
      else
        raise "`rule` expects a block with signature |node| or |node, children|"
      end
    end
  end

  class PEGLanguage < Language
    # grammar <- spacing definition+
    rule Rule[:grammar,
              Sequence[:spacing, OneOrMore[:definition]]] do |node, children|
      children[1]
    end

    # definition <- identifier left_arrow expression
    rule Rule[:definition, Sequence[:identifier,
                                    :left_arrow,
                                    :expression]] do |node, children|
      identifier, _, expression = children
      Rule[identifier, expression]
    end

    # expression <- sequence (slash sequence)*
    rule Rule[:expression,
              Sequence[:sequence,
                       ZeroOrMore[Sequence[:slash,
                                           :sequence]]]] do |node, children|
      sequence, rest = children
      rest = rest.map { |slash, sequence| sequence }
      rest.length == 0 ? sequence : Or[sequence, *rest]
    end

    # sequence <- prefix*
    rule Rule[:sequence, ZeroOrMore[:prefix]] do |node, children|
      children.length == 1 ? children[0] : Sequence[*children]
    end

    # prefix <- (and / not)? suffix
    rule Rule[:prefix,
              Sequence[Optional[Or[:and, :not]], :suffix]] do |node, children|
      suffix = children[1]
      prefix = node.children[0].text.strip  # HACK
      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    # suffix <- primary (question / star / plus)?
    rule Rule[:suffix,
              Sequence[:primary, Optional[Or[:question,
                                             :star,
                                             :plus]]]] do |node, children|
      primary = children[0]
      suffix = node.children[1].text.strip  # HACK
      { '' => primary,
        '?' => Optional[primary],
        '*' => ZeroOrMore[primary],
        '+' => OneOrMore[primary],
      }.fetch(suffix)
    end

    # _identifier_value <- identifier !left_arrow
    rule Rule[:_identifier_value,
              Sequence[:identifier, Not[:left_arrow]]] do |node, children|
      children[0]
    end

    # _parenthesised <- open expression close
    rule Rule[:_parenthesised,
              Sequence[:open, :expression, :close]] do |node, children|
      children[1]
    end

    # primary <- _identifier_value / _parenthesised / literal / class / dot
    rule Rule[:primary, Or[:_identifier_value, :_parenthesised,
                           :literal, :class, :dot]] do |node, children|
      children[0]
    end

    # identifier = [A-Za-z0-9_]+ spacing  # HACK simplified
    rule Rule[:identifier,
              Sequence[Regex['[A-Za-z0-9_]+'], :spacing]] do |node, children|
      identifier_regex = node.children[0].text
      identifier_regex.to_sym
    end

    # class <- '[' ... ']' spacing  # HACK simplified
    rule Rule[:class, Sequence[Regex['\[.*?\]'], :spacing]] do |node, children|
      Regex[node.text.strip]
    end

    # literal <- (['] ... ['] / ["] ... ["]) spacing  # HACK simplified
    rule Rule[:literal, Sequence[Or[Regex["'.*?'"], Regex['".*?"']],
                                 :spacing]] do |node, children|
      Literal[Kernel.eval(node.text)]
    end

    # dot <- '.' spacing
    rule Rule[:dot, Sequence[Literal['.'], :spacing]] do |node, children|
      Regex['.']
    end

    def self.token(source)
      /(?<name>\S+) *<- "(?<value>\S+)" spacing/ =~ source
      rule Rule[name.to_sym, Sequence[Literal[value], :spacing]]
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
    rule Rule[:spacing, ZeroOrMore[Or[:space, :comment]]]

    # comment <- '#' (!end_of_line .)* end_of_line
    rule Rule[:comment,
              Sequence[Literal['#'], ZeroOrMore[Sequence[Not[:end_of_line],
                                                         Regex['.']]]]]

    # space <- " " / "\t" / end_of_line
    rule Rule[:space, Or[Literal[" "], Literal["\t"], :end_of_line]]

    # end_of_line <- "\r\n" / "\n" / "\r"
    rule Rule[:end_of_line, Or[Literal["\r\n"], Literal["\n"], Literal["\r"]]]
  end
end
