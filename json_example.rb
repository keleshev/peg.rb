require './peg'

class JSONLanguage < PEG::Language
  rule 'number <- [0-9]+' do |node, children|
    node[:text].to_i
  end
end

JSON = JSONLanguage.new
