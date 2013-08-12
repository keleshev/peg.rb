peg
===

**peg** is a gem that implements Parsing Expression Grammar
(PEG) as it is described in Bryan Ford's [original paper](
http://pdos.csail.mit.edu/papers/parsing:popl04.pdf).


Installation
------------

    gem install peg

API
---

### `PEG::Grammar`

#### Example

```ruby
grammar = PEG::Grammar.new('
  value  <- number / expr
  number <- [0-9]+
  expr   <- "(" sum ")"
  prod   <- value ("*" value)*
  sum    <- prod ("+" prod)*
')

grammar.parse('(2+3*4)') #=> PEG::Node.new("(2*3+4)", ..., "value")
```

#### `PEG::Grammar.new(source) → grammar`

Takes a `source` string written in **PEG notation** and
returns a new `Grammar` object.

#### `grammar.parse(source) → node`

Parses `source` string into an abstract syntax tree and
returns the top `PEG::Node` object of that tree.

### `PEG::Language`

While `PEG::Grammar` captures only the sytax rules,
`PEG::Language` allows you to associate each syntax rule
with an action.

`PEG::Language` is an abastract class, it is intended to be
subclassed.

#### Example

```ruby
class SimpleMath < PEG::Language
  rule('value  <- number / expr') { |node, children| children[0] }
  rule('expr   <- "(" sum ")"')   { |node, children| children[1] }
  rule('number <- [0-9]+')        { |node| node.text.to_i }

  rule('prod <- value ("*" value)*') do |node, children|
    children.flatten.compact.inject('*')
  end

  rule('sum <- prod ("+" prod)*') do |node, children|
    children.flatten.compact.inject('+')
  end
end

SimpleMath.new.eval('(2+3*4)') #=> 14
```

#### `PEG::Language.rule(source) { |node, children| block }`
#### `PEG::Language.rule(source) { |node| block }`
#### `PEG::Language.rule(source)`

`PEG::Language.rule` is a class method intended to be put
inside a class body.  It takes a `source` string which
should be a single grammar rule in **PEG notation**.

Given a block that takes **one** argument, the block will
be executed every time the rule matches.  Abstract syntax
tree `node` will be passed to it.

Given a block that takes **two** arguments, the block will
also receive `children`, which are recursively evaluated
components of the rule.

### `language.eval(source)`

Parses the `source` string, then visits each node and runs
the blocks associated with each rule.

### `PEG::Node`

Abstract syntax tree node.  Such nodes are created during
parsing step.  Creating them manually is not part of the
public API.

#### Example

```ruby
node = grammar.parse('(2+3*4)')

node.text     #=> '(2*3+4)'
node.children #=> [PEG:Node(..., 'expr')]
node.name     #=> 'value'
```

#### `node.text`

Text from which that node was parsed.

#### `node.name`

Name of the grammar rule that created that node.

#### `node.children`

Possibly empty list of child branches of that node.
