require_relative '../examples/math_example'


describe SimpleMath do
  it 'computes' do
    SimpleMath.new.eval('(2+3+4)').should == 9
    SimpleMath.new.eval('(2+3*4)').should == 14
    SimpleMath.new.eval('(2*3+4)').should == 10
    SimpleMathGrammar.parse('(2*3+4)')
  end
end
