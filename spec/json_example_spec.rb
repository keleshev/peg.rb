require_relative '../examples/json_example'


describe JSONLanguage do
  it 'has numbers' do
    JSON.eval('42').should == 42
    JSON.eval('-1.2e-3') == -1.2e-3
  end

  it 'has strings' do
    JSON.eval('"ab"').should == 'ab'
  end

  it 'has boolean' do
    JSON.eval('true').should == true
    JSON.eval('false').should == false
    JSON.eval('null').should == nil
  end

  it 'has arrays' do
    JSON.eval('[]').should == []
    JSON.eval('[42]').should == [42]
    JSON.eval('[42,2,1]').should == [42, 2, 1]
  end

  it 'has objects' do
    JSON.eval('{}').should == {}
    JSON.eval('{"a":1}').should == {"a" => 1}
    JSON.eval('{"a":1,"b":2,"c":3}').should == {"a" => 1, "b" => 2, "c" => 3}
  end
end
