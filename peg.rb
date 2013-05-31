module PEG
  class Rule
    attr_accessor :children

    def name(value=nil)
      if value
        @name = value
        self
      else
        @name
      end
    end

    def result(text, children=nil)
      res = {text: text}
      children ? res.merge!({children: children}) : res
      @name ? res.merge!({name: @name}) : res
    end

    def inspect
      repr = "#{self.class}.new(#{self._inspect})"
      @name ? repr + ".name(#{@name.inspect})" : repr
    end

    def ==(other)
      self.inspect == other.inspect
    end
  end

  class Literal < Rule
    def initialize(literal)
      @literal = literal
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
    def initialize(*children)
      @children = children
    end

    def match(text)
      text_ = String.new(text)
      len = 0
      children = []
      @children.each do |member|
        res = member.match(text_)
        if res == nil
          return nil
        else
          children << res
          text_ = text_.slice res[:text].length..text_.length
          len += res[:text].length
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
      @children.each do |member|
        res = member.match(text)
        return result(res[:text], [res]) if res
      end
      nil
    end
  end

  class Not < Sequence
    def match(text)
      @children[0].match(text) ? nil : result('')
    end
  end

  class And < Not
    def match(text)
      @children[0].match(text) ? result('') : nil
    end
  end

  class OneOrMore < Not
    def range
      (1..Float::INFINITY)
    end

    def match(text)
      text_ = String.new(text)
      len = 0
      children = []
      loop do
        res = @children[0].match(text_)
        break if not res
        children << res
        break if res[:text] == ''
        text_ = text_.slice res[:text].length..text_.length
        len += res[:text].length
      end
      range.include?(children.length) ? result(text.slice(0...len),
                                               children) : nil
    end
  end

  class ZeroOrMore < OneOrMore
    def range
      (0..Float::INFINITY)
    end
  end

  class Optional < OneOrMore
    def range
      (0..1)
    end
  end

  class Reference < Rule
    attr_reader :reference

    def initialize(name)
      @reference = name
    end

    def _inspect
      @reference.inspect
    end
  end

  class Grammar
    def initialize(source)
      @_grammar = node = self.class._peg.match(source)
      if @_grammar[:text].length != source.length
        raise SyntaxError.new source[@_grammar[:text].length, 50].inspect
      end
    end

    def parse(source)
      res = self.grammar[0].match(source)
      if res[:text].length != source.length
        raise SyntaxError.new source[res[:text].length, 50].inspect
      else
        res
      end
    end

    def _parse(node)
      return node if node[:name] == nil
      self.send('visit_' + node[:name], node,
                node[:children] ? node[:children].map {|c| _parse(c)} : nil)
    end

    def grammar
      self._parse(@_grammar)
    end

    def visit_identifier__regex(node, children)
      node[:text]
    end

    def visit_identifier(node, children)
      identifier_regex, spacing = children
      Reference.new(identifier_regex)
    end

    def visit_literal(node, children)
      Literal.new(eval(node[:text]))
    end

    def visit_dot(node, children)
      Regex.new('.')
    end

    def visit_class(node, children)
      class_, spacing = children
      Regex.new(class_[:text])
    end

    def visit_definition(node, children)
      identifier, left_arrow, expression = children
      expression.name(identifier.reference)
    end

    def visit_expression(node, children)
      sequence, rest = children
      if rest.length == 0
        sequence
      else
        Or.new(sequence, *rest)
      end
    end

    def visit_expression__zeroormore(node, children)
      children
    end

    def visit_expression__sequence(node, children)
      slash, sequence = children
      sequence
    end

    def visit_grammar(node, children)
      spacing, definitions = children
      definitions
    end

    def visit_grammar__oneormore(node, children)
      children
    end

    def visit_primary(node, children)
      children[0]
    end

    def visit_primary__sequence(node, children)
      identifier, not_left_arrow = children
      identifier
    end

    def visit_primary__parens(node, children)
      open, expression, close = children
      expression
    end

    def visit_prefix__optional(node, children)
      node[:text].strip  # HACK
    end

    def visit_prefix(node, children)
      prefix, suffix = children
      prefix == '' ? suffix : {'&' => And, '!' => Not}.fetch(prefix).new(suffix)
    end

    def visit_sequence(node, children)
      children.length == 1 ? children[0] : Sequence.new(*children)
    end

    def visit_suffix__optional(node, children)
      node[:text].strip  # HACK
    end

    def visit_suffix(node, children)
      primary, optional_suffix = children
      optional_suffix == '' ? primary : {
        '?' => Optional,
        '*' => ZeroOrMore,
        '+' => OneOrMore,
      }.fetch(optional_suffix).new(primary)
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
      elsif rule.children && rule.children.length > 0
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
    def self.rule(rule, &block)
      @@rule = rule
      @@block = block
    end

    def eval(source)
      #p [@@rule, @@block]
      node = Grammar.new(@@rule).parse(source)
      children = 'hai'
      self.instance_exec(node, children, &@@block)
    end
  end
end
