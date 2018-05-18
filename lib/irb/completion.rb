# frozen_string_literal: false
#
#   irb/completor.rb -
#   	$Release Version: 0.9$
#   	$Revision$
#   	by Keiju ISHITSUKA(keiju@ishitsuka.com)
#       From Original Idea of shugo@ruby-lang.org
#

require "readline"
require "rdoc"

module IRB
  module InputCompletor # :nodoc:

    RDocRIDriver = RDoc::RI::Driver.new

    # Set of reserved words used by Ruby, you should not use these for
    # constants or variables
    ReservedWords = %w[
      BEGIN END
      alias and
      begin break
      case class
      def defined do
      else elsif end ensure
      false for
      if in
      module
      next nil not
      or
      redo rescue retry return
      self super
      then true
      undef unless until
      when while
      yield
    ]

    CompletionProc = proc { |input|
      workspace = IRB.conf[:MAIN_CONTEXT].workspace
      if workspace.completion_data[:prev] == input
        workspace.completion_data[:repeat_count] += 1
      else
        workspace.completion_data[:repeat_count] = 0
      end
      workspace.completion_data[:prev] = input

      candidates = complement(input)
      if workspace.completion_data[:display_name]
        display_name = workspace.completion_data[:display_name]
        workspace.completion_data[:display_name] = nil
        begin
          RDocRIDriver.display_name(display_name)
        rescue RDoc::RI::Driver::NotFoundError
          candidates
        else
          []
        end
      else
        candidates
      end
    }

    def self.check_display_name(candidates, on_input, receiver = nil)
      ws = IRB.conf[:MAIN_CONTEXT].workspace
      if ws.completion_data[:repeat_count] > 1 and candidates.find{|m| m == on_input}
        if block_given?
          ws.completion_data[:display_name] = yield
        else
          ws.completion_data[:display_name] = receiver ? "#{receiver}##{on_input}" : on_input
        end
      end
      candidates
    end

    def self.complement(input)
      bind = IRB.conf[:MAIN_CONTEXT].workspace.binding
      case input
      when /^((["'`]).*\2)\.([^.]*)$/
        # String
        receiver = $1
        on_input = $3
        message = Regexp.quote($3)

        candidates = String.instance_methods.collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates)
        check_display_name(candidates, input) do
          "String##{on_input}"
        end

      when /^(\/[^\/]*\/)\.([^.]*)$/
        # Regexp
        receiver = $1
        on_input = $2
        message = Regexp.quote($2)

        candidates = Regexp.instance_methods.collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates)
        check_display_name(candidates, input) do
          "Regexp##{on_input}"
        end

      when /^([^\]]*\])\.([^.]*)$/
        # Array
        receiver = $1
        on_input = $2
        message = Regexp.quote($2)

        candidates = Array.instance_methods.collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates)
        check_display_name(candidates, input) do
          "Array##{on_input}"
        end

      when /^([^\}]*\})\.([^.]*)$/
        # Proc or Hash
        receiver = $1
        on_input = $2
        message = Regexp.quote($2)

        candidates = Proc.instance_methods.collect{|m| m.to_s}
        candidates |= Hash.instance_methods.collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates)
        check_display_name(candidates, input) do
          if Proc.instance_methods.find{|m| m.to_s == on_input}
            "Proc##{on_input}"
          elsif Hash.instance_methods.find{|m| m.to_s == on_input}
            "Hash##{on_input}"
          end
        end

      when /^(:[^:.]*)$/
        # Symbol
        if Symbol.respond_to?(:all_symbols)
          sym = $1
          candidates = Symbol.all_symbols.collect{|s| ":" + s.id2name}
          candidates.grep(/^#{Regexp.quote(sym)}/)
        else
          []
        end

      when /^::([A-Z][^:\.\(]*)$/
        # Absolute Constant or class methods
        receiver = $1
        candidates = Object.constants.collect{|m| m.to_s}
        candidates = candidates.grep(/^#{receiver}/).collect{|e| "::" + e}
        check_display_name(candidates, input) do
          receiver
        end

      when /^([A-Z].*)::([^:.]*)$/
        # Constant or class methods
        receiver = $1
        on_input = $2
        message = Regexp.quote($2)
        begin
          candidates = eval("#{receiver}.constants.collect{|m| m.to_s}", bind)
          candidates |= eval("#{receiver}.methods.collect{|m| m.to_s}", bind)
        rescue Exception
          candidates = []
        end
        candidates = select_message(receiver, message, candidates, "::")
        check_display_name(candidates, input) do
          input
        end

      when /^(:[^:.]+)(\.|::)([^.]*)$/
        # Symbol
        receiver = $1
        sep = $2
        on_input = $3
        message = Regexp.quote($3)

        candidates = Symbol.instance_methods.collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates, sep)
        check_display_name(candidates, on_input, "Symbol")

      when /^(-?(0[dbo])?[0-9_]+(\.[0-9_]+)?([eE]-?[0-9]+)?)(\.|::)([^.]*)$/
        # Numeric
        receiver = $1
        sep = $5
        on_input = $6
        message = Regexp.quote($6)

        begin
          candidates = eval(receiver, bind).methods.collect{|m| m.to_s}
        rescue Exception
          candidates = []
        end
        candidates = select_message(receiver, message, candidates, sep)
        check_display_name(candidates, input) do
          rec = eval(receiver, bind)
          "#{rec.class.name}##{on_input}"
        end

      when /^(-?0x[0-9a-fA-F_]+)(\.|::)([^.]*)$/
        # Numeric(0xFFFF)
        receiver = $1
        sep = $2
        on_input = $3
        message = Regexp.quote($3)

        begin
          candidates = eval(receiver, bind).methods.collect{|m| m.to_s}
        rescue Exception
          candidates = []
        end
        candidates = select_message(receiver, message, candidates, sep)
        check_display_name(candidates, input) do
          rec = eval(receiver, bind)
          display_name = "#{rec.class.name}##{on_input}"
        end

      when /^(\$[^.]*)$/
        # global var
        regmessage = Regexp.new(Regexp.quote($1))
        candidates = global_variables.collect{|m| m.to_s}.grep(regmessage)

      when /^([^."].*)(\.|::)([^.]*)$/
        # variable.func or func.func
        receiver = $1
        sep = $2
        on_input = $3
        message = Regexp.quote($3)

        gv = eval("global_variables", bind).collect{|m| m.to_s}
        lv = eval("local_variables", bind).collect{|m| m.to_s}
        iv = eval("instance_variables", bind).collect{|m| m.to_s}
        cv = eval("self.class.constants", bind).collect{|m| m.to_s}

        if (gv | lv | iv | cv).include?(receiver) or /^[A-Z]/ =~ receiver && /\./ !~ receiver
          # foo.func and foo is var. OR
          # foo::func and foo is var. OR
          # foo::Const and foo is var. OR
          # Foo::Bar.func
          begin
            candidates = []
            rec = eval(receiver, bind)
            if sep == "::" and rec.kind_of?(Module)
              candidates = rec.constants.collect{|m| m.to_s}
            end
            candidates |= rec.methods.collect{|m| m.to_s}
            check_display_name(candidates, on_input) do
              if sep == "::" or rec.kind_of?(Module)
                "#{rec.name}::#{on_input}"
              else
                "#{rec.class.name}##{on_input}"
              end
            end
          rescue Exception
            candidates = []
          end
        else
          # func1.func2
          candidates = []
          to_ignore = ignored_modules
          ObjectSpace.each_object(Module){|m|
            next if (to_ignore.include?(m) rescue true)
            candidates.concat m.instance_methods(false).collect{|x| x.to_s}
          }
          candidates.sort!
          candidates.uniq!
          check_display_name(candidates, on_input) do
            "#{sep}#{on_input}"
          end
        end
        select_message(receiver, message, candidates, sep)

      when /^\.([^.]*)$/
        # unknown(maybe String)

        receiver = ""
        on_input = $1
        message = Regexp.quote($1)

        candidates = String.instance_methods(true).collect{|m| m.to_s}
        candidates = select_message(receiver, message, candidates)
        check_display_name(candidates, on_input, "String")

      else
        candidates = eval("methods | private_methods | local_variables | instance_variables | self.class.constants", bind).collect{|m| m.to_s}

        candidates = (candidates | ReservedWords).grep(/^#{Regexp.quote(input)}/)
        check_display_name(candidates, input) do
          "Kernel##{input}"
        end
      end
    end

    # Set of available operators in Ruby
    Operators = %w[% & * ** + - / < << <= <=> == === =~ > >= >> [] []= ^ ! != !~]

    def self.select_message(receiver, message, candidates, sep = ".")
      candidates.grep(/^#{message}/).collect do |e|
        case e
        when /^[a-zA-Z_]/
          receiver + sep + e
        when /^[0-9]/
        when *Operators
          #receiver + " " + e
        end
      end
    end

    def self.ignored_modules
      # We could cache the result, but this is very fast already.
      # By using this approach, we avoid Module#name calls, which are
      # relatively slow when there are a lot of anonymous modules defined.
      s = {}

      scanner = lambda do |m|
        next if s.include?(m) # IRB::ExtendCommandBundle::EXCB recurses.
        s[m] = true
        m.constants(false).each do |c|
          value = m.const_get(c)
          scanner.call(value) if value.is_a?(Module)
        end
      end

      %i(IRB SLex RubyLex RubyToken).each do |sym|
        next unless Object.const_defined?(sym)
        scanner.call(Object.const_get(sym))
      end

      s.delete(IRB::Context) if defined?(IRB::Context)

      s
    end
  end
end

if Readline.respond_to?("basic_word_break_characters=")
  Readline.basic_word_break_characters= " \t\n`><=;|&{("
end
Readline.completion_append_character = nil
Readline.completion_proc = IRB::InputCompletor::CompletionProc
