module PEG
  class AbstractValue
    def ==(other)
      inspect == other.inspect
    end

    def inspect
      "#{self.class}.new(#{arguments.map(&:inspect).join(', ')})"
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
      text.start_with?(@literal) ? Node.new(@literal) : nil
    end

    def arguments
      [@literal]
    end
  end

  class Regex < Literal
    def match(text)
      res = Regexp.new('\A' + @literal).match(text)
      res && Node.new(res.to_s)
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
      Node.new(text.slice(0...len), children)
    end

    def arguments
      @children
    end
  end

  class Or < Sequence
    def match(text)
      @children.each do |child|
        node = child.match(text)
        return Node.new(node.text, [node]) if node
      end
      nil
    end
  end

  class Not < Sequence
    def match(text)
      @children[0].match(text) ? nil : Node.new('')
    end
  end

  class And < Sequence
    def match(text)
      @children[0].match(text) ? Node.new('') : nil
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
      in_range ? Node.new(text.slice(0...len), children) : nil
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

  class PEGLanguage < PEG::Language
    # grammar <- spacing definition+
    _ = Rule.new(:grammar, Sequence.new(:spacing, OneOrMore.new(:definition)))
    rule _ do |node, children|
      spacing, definitions = children
      definitions
    end

    # definition <- identifier left_arrow expression
    _ = Rule.new(:definition,
                 Sequence.new(:identifier, :left_arrow, :expression))
    rule _ do |node, children|
      identifier, left_arrow, expression = children
      Rule.new(identifier, expression)
    end

    # expression <- sequence (slash sequence)*
    _ = Rule.new(:expression,
                 Sequence.new(:sequence,
                              ZeroOrMore.new(Sequence.new(:slash, :sequence))))
    rule _ do |node, children|
      sequence, rest = children
      rest = rest.map { |slash, sequence| sequence }
      rest.length == 0 ? sequence : Or.new(sequence, *rest)
    end

    # sequence <- prefix*
    rule Rule.new(:sequence, ZeroOrMore.new(:prefix)) do |node, children|
      children.length == 1 ? children[0] : Sequence.new(*children)
    end

    # prefix <- (and / not)? suffix
    _ = Rule.new(:prefix,
                 Sequence.new(Optional.new(Or.new(:and, :not)), :suffix))
    rule _ do |node, children|
      suffix = children[1]
      prefix = node.children[0].text.strip  # HACK
      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    # suffix <- primary (question / star / plus)?
    _ = Rule.new(:suffix,
         Sequence.new(:primary, Optional.new(Or.new(:question, :star, :plus))))
    rule _ do |node, children|
      primary = children[0]
      suffix = node.children[1].text.strip  # HACK
      { '' => primary,
        '?' => Optional.new(primary),
        '*' => ZeroOrMore.new(primary),
        '+' => OneOrMore.new(primary),
      }.fetch(suffix)
    end

    # _identifier_value <- identifier !left_arrow
    _ = Rule.new(:_identifier_value,
         Sequence.new(:identifier, Not.new(:left_arrow)))
    rule _ do |node, children|
      identifier, not_left_arrow = children
      identifier
    end

    # _parenthesised <- open expression close
    _ = Rule.new(:_parenthesised, Sequence.new(:open, :expression, :close))
    rule _ do |node, children|
      open, expression, close = children
      expression
    end

    # primary <- _identifier_value / _parenthesised / literal / class / dot
    _ = Rule.new(:primary, Or.new(:_identifier_value, :_parenthesised,
                                  :literal, :class, :dot))
    rule _ do |node, children|
      children[0]
    end

    # identifier = [A-Za-z0-9_]+ spacing  # HACK simplified
    _ = Rule.new(:identifier,
                 Sequence.new(Regex.new('[A-Za-z0-9_]+'), :spacing))
    rule _ do |node, children|
      identifier_regex = node.children[0].text
      identifier_regex.to_sym
    end

    # class <- '[' ... ']' spacing  # HACK simplified
    _ = Rule.new(:class, Sequence.new(Regex.new('\[.*?\]'), :spacing))
    rule _ do |node, children|
      Regex.new(node.text.strip)
    end

    # literal <- (['] ... ['] / ["] ... ["]) spacing  # HACK simplified
    _ = Rule.new(:literal, Sequence.new(Or.new(Regex.new("'.*?'"),
                                               Regex.new('".*?"')), :spacing))
    rule _ do |node, children|
      Literal.new(Kernel.eval(node.text))
    end

    # dot <- '.' spacing
    _ = Rule.new(:dot, Sequence.new(Literal.new('.'), :spacing))
    rule(_) { |node, children| Regex.new('.') }

    def self.token(source)
      /(?<name>\S+) *<- "(?<value>\S+)" spacing/ =~ source
      rule(Rule.new(name.to_sym,
                    Sequence.new(Literal.new(value),
                                 :spacing))) { |node, children| node }
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

    node = proc { |node, children| node }

    # spacing <- (space / comment)*
    rule Rule.new(:spacing, ZeroOrMore.new(Or.new(:space, :comment))), &node

    # comment <- '#' (!end_of_line .)* end_of_line
    rule Rule.new(:comment,
          Sequence.new(Literal.new('#'),
                       ZeroOrMore.new(Sequence.new(Not.new(:end_of_line),
                                                   Regex.new('.'))))), &node

    # space <- " " / "\t" / end_of_line
    rule Rule.new(:space, Or.new(Literal.new(" "),
                                 Literal.new("\t"),
                                 :end_of_line)), &node

    # end_of_line <- "\r\n" / "\n" / "\r"
    rule Rule.new(:end_of_line, Or.new(Literal.new("\r\n"),
                                       Literal.new("\n"),
                                       Literal.new("\r"))), &node
  end
end
