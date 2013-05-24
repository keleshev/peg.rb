class Literal
  def initialize(literal)
    @literal = literal
  end

  def match(text)
    text.start_with?(@literal) ? @literal.length : nil
  end
end

class Regex < Literal
  def match(text)
    match = Regexp.new('^' + @literal).match(text)
    match && match.to_s.length
  end
end

class Sequence
  def initialize(*children)
    @children = children
  end

  def match(text)
    length = 0
    @children.each do |member|
      match = member.match(text)
      if match == nil
        return nil
      else
        text = text.slice match..text.length
        length += match
      end
    end
    length
  end
end

class Or < Sequence
  def match(text)
    @children.each do |member|
      match = member.match(text)
      return match if match
    end
    nil
  end
end

class Not
  def initialize(child)
    @child = child
  end

  def match(text)
    @child.match(text) ? nil : 0
  end
end

class And < Not
  def match(text)
    @child.match(text) ? 0 : nil
  end
end

class Optional < Not
  def match(text)
    @child.match(text) || 0
  end
end

class OneOrMore < Not
  def range
    (1..Float::INFINITY)
  end

  def match(text)
    length = 0
    count = 0
    loop do
      match = @child.match(text)
      break if not match
      count += 1
      break if match == 0
      text = text.slice match..text.length
      length += match
    end
    range.include?(count) ? length : nil
  end
end

class ZeroOrMore < OneOrMore
  def range
    (0..Float::INFINITY)
  end
end
