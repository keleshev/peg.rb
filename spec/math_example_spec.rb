require_relative '../examples/math_example'


describe SimpleMath do
  it 'computes' do
    SimpleMath.eval('(2+3+4)').should == 9
    SimpleMath.eval('(2+3*4)').should == 14
    SimpleMath.eval('(2*3+4)').should == 10
    SimpleMathGrammar[:value].match('(2*3+4)')
  end
end
