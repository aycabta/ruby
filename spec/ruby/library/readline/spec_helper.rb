require_relative '../../spec_helper'

begin
  require 'readline'
rescue LoadError
else
  # rb-readline and reline behaves quite differently
  unless defined?(RbReadline) or defined?(Reline)
    MSpec.enable_feature :readline
  end
end
