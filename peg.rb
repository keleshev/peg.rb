module PEG
  class ValueObject
    def ==(other)
      self.inspect == other.inspect
    end
  end

  class Node < ValueObject
    attr_accessor :text, :children, :name

    def initialize(text, children=[], name=nil)
      @text, @children, @name = text, children, name
    end

    def inspect
      "#{self.class}.new(#{text.inspect}, #{children.inspect}, #{name.inspect})"
    end
  end

  class Rule < ValueObject
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

    def result(text, children=[])
      Node.new(text, children, @name)
    end

    def inspect
      repr = "#{self.class}.new(#{self._inspect})"
      @name ? repr + ".name(#{@name.inspect})" : repr
    end
  end

  class Literal < Rule
    def initialize(literal)
      @literal = literal
      @children = []
    end

    def match(text)
      text.start_with?(@literal) ? result(@literal) : nil
    end

    def _inspect
      @literal.inspect
    end
  end

  class Regex < Literal
    def match(text)
      res = Regexp.new('\A' + @literal).match(text)
      res && result(res.to_s)
    end
  end

  class Sequence < Rule
    def match(text)
      text_ = String.new(text)
      len = 0
      children = []
      @children.each do |child|
        node = child.match(text_)
        if node == nil
          return nil
        else
          children << node
          text_ = text_.slice node.text.length..text_.length
          len += node.text.length
        end
      end
      result(text.slice(0...len), children)
    end

    def _inspect
      @children.map(&:inspect).join(', ')
    end
  end

  class Or < Sequence
    def match(text)
      @children.each do |child|
        node = child.match(text)
        return result(node.text, [node]) if node
      end
      nil
    end
  end

  class Not < Sequence
    def match(text)
      @children[0].match(text) ? nil : result('')
    end
  end

  class And < Sequence
    def match(text)
      @children[0].match(text) ? result('') : nil
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
      in_range ? result(text.slice(0...len), children) : nil
    end
  end

  class ZeroOrMore < OneOrMore
    @range = (0..Float::INFINITY)
  end

  class Optional < OneOrMore
    @range = (0..1)
  end

  class Reference < Rule
    attr_reader :reference

    def initialize(name)
      @reference = name
      @children = []
    end

    def _inspect
      @reference.inspect
    end
  end

  class Visitor
    def self.visit(node)
      return node if node.name == nil
      self.send('visit_' + node.name, node, node.children.map {|c| visit(c)})
    end
  end

  class GrammarGenerator < Visitor
    def self.visit_identifier__regex(node, children)
      node.text
    end

    def self.visit_identifier(node, children)
      identifier_regex, spacing = children
      Reference.new(identifier_regex)
    end

    def self.visit_literal(node, children)
      Literal.new(Kernel.eval(node.text))
    end

    def self.visit_dot(node, children)
      Regex.new('.')
    end

    def self.visit_class(node, children)
      class_, spacing = children
      Regex.new(class_.text)
    end

    def self.visit_definition(node, children)
      identifier, left_arrow, expression = children
      expression.name(identifier.reference)
    end

    def self.visit_expression(node, children)
      sequence, rest = children
      rest.length == 0 ? sequence : Or.new(sequence, *rest)
    end

    def self.visit_expression__zeroormore(node, children)
      children
    end

    def self.visit_expression__sequence(node, children)
      slash, sequence = children
      sequence
    end

    def self.visit_grammar(node, children)
      spacing, definitions = children
      definitions
    end

    def self.visit_grammar__oneormore(node, children)
      children
    end

    def self.visit_primary(node, children)
      children[0]
    end

    def self.visit_primary__sequence(node, children)
      identifier, not_left_arrow = children
      identifier
    end

    def self.visit_primary__parens(node, children)
      open, expression, close = children
      expression
    end

    def self.visit_prefix__optional(node, children)
      node.text.strip  # HACK
    end

    def self.visit_prefix(node, children)
      prefix, suffix = children
      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    def self.visit_sequence(node, children)
      children.length == 1 ? children[0] : Sequence.new(*children)
    end

    def self.visit_suffix__optional(node, children)
      node.text.strip  # HACK
    end

    def self.visit_suffix(node, children)
      primary, optional_suffix = children
      optional_suffix == '' ? primary : {
        '?' => Optional,
        '*' => ZeroOrMore,
        '+' => OneOrMore,
      }.fetch(optional_suffix).new(primary)
    end
  end

  class Grammar
    def initialize(source)
      @_grammar = node = self.class._peg.match(source)
      if @_grammar.text.length != source.length
        raise SyntaxError.new source[@_grammar.text.length, 50].inspect
      end
    end

    def parse(source)
      res = self.grammar[0].match(source)
      if res.text.length != source.length
        raise SyntaxError.new source[res.text.length, 50].inspect
      else
        res
      end
    end

    def grammar
      GrammarGenerator.visit(@_grammar)
    end

    def self._peg
      end_of_line = Or.new(
                      Literal.new("\r\n"),
                      Literal.new("\n"),
                      Literal.new("\r"),
                    )
      space = Or.new(Literal.new(" "), Literal.new("\t"), end_of_line)
      comment = Sequence.new(
                  Literal.new('#'),
                  ZeroOrMore.new(
                    Sequence.new(Not.new(end_of_line), Regex.new('.')),
                  ),
                  end_of_line,
                )
      spacing = ZeroOrMore.new(Or.new(space, comment))

      and_ = Sequence.new(Literal.new('&'), spacing)
      not_ = Sequence.new(Literal.new('!'), spacing)
      slash = Sequence.new(Literal.new('/'), spacing)
      left_arrow = Sequence.new(Literal.new('<-'), spacing)
      question = Sequence.new(Literal.new('?'), spacing)
      star = Sequence.new(Literal.new('*'), spacing)
      plus = Sequence.new(Literal.new('+'), spacing)
      open = Sequence.new(Literal.new('('), spacing)
      close = Sequence.new(Literal.new(')'), spacing)
      dot = Sequence.new(Literal.new('.'), spacing).name('dot')

      # HACK these three rules are simplified
      literal = Sequence.new(
                  Or.new(Regex.new("'.*?'"), Regex.new('".*?"')),
                  spacing
                ).name('literal')
      class_ = Sequence.new(Regex.new('\[.*?\]'), spacing).name('class')
      identifier = Sequence.new(
                     Regex.new('[A-Za-z0-9_]+').name('identifier__regex'),
                     spacing
                   ).name('identifier')

      primary = Or.new(
                  Sequence.new(
                    identifier,
                    Not.new(left_arrow)
                  ).name('primary__sequence'),
                  Sequence.new(
                    open,
                    'EXPRESSION',  # paceholder for future substitution
                    close
                  ).name('primary__parens'),
                  literal,
                  class_,
                  dot,
                ).name('primary')
      suffix = Sequence.new(
                 primary,
                 Optional.new(
                   Or.new(question, star, plus)
                 ).name('suffix__optional'),
               ).name('suffix')
      prefix = Sequence.new(
                 Optional.new(
                   Or.new(and_, not_)
                 ).name('prefix__optional'),
                 suffix
               ).name('prefix')
      sequence = ZeroOrMore.new(prefix).name('sequence')
      expression = Sequence.new(
                     sequence,
                     ZeroOrMore.new(
                       Sequence.new(
                         slash,
                         sequence
                       ).name('expression__sequence')
                     ).name('expression__zeroormore')
                   ).name('expression')
      if primary.children[1].children[1] != 'EXPRESSION'
        raise 'Invalid PEG grammar'
      else
        primary.children[1].children[1] = expression
      end
      definition = Sequence.new(
                     identifier,
                     left_arrow,
                     expression
                   ).name('definition')
      # In the original PEG paper `grammar` is specified as:
      #     grammar <- spacing definition+ end_of_file
      # but we skip `end_of_file` allowing the grammar to
      # match just a part of source in order to know where
      # the syntax error occured.
      grammar = Sequence.new(
                  spacing,
                  OneOrMore.new(definition).name('grammar__oneormore')
                ).name('grammar')

      grammar
    end
  end

  class ReferenceResolver
    def initialize(rules)
      @rules = rules
    end

    def resolve
      _resolve!(@rules[0])
    end

    def _resolve!(rule)
      if rule.class == Reference
        resolved_rule = reference(rule.reference)
        if resolved_rule.class == Reference
          _resolve!(resolved_rule)
        else
          resolved_rule
        end
      elsif rule.children.length > 0
        rule.children.map! {|child| _resolve!(child)}
        rule
      else
        rule
      end
    end

    def reference(name)
      @rules.find {|r| r.name == name} || raise("rule `#{name}` not found")
    end
  end

  class Language
    @@default = proc {|node, children| children}
    # we rely on the fact that 1.9+ Hash maintains order
    @@rules = {}
    @@blocks = {}

    def self.rule(rule, &block)
      name = rule.split('<-')[0].strip
      @@rules[name] = rule
      @@blocks[name] = block
    end

    def self.default(&block)
      @@default = block
    end

    def eval(source)
      grammar_source = @@rules.values.join("\n")
      grammar_list = Grammar.new(grammar_source).grammar
      grammar = ReferenceResolver.new(grammar_list).resolve
      node = grammar.match(source)
      if node.text.length != source.length
        raise SyntaxError.new source[node.text.length, 50].inspect
      end
      _eval(node)
    end

    def _eval(node)
      block = node.name ? @@blocks.fetch(node.name) : @@default
      children = node.children.map {|child| self._eval(child)}
      self.instance_exec(node, children, &block)
    end
  end
end
