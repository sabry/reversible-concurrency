require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 #
 #  Inside "choose"
 #      prints iteration and arg shifted in after choose
 #      increments timestamp
 # 
 #      after j iterations, backtracks
 #
 #  Final backtrack throws an exception since
 #  it is outside of the scope of a choose.
 3
 #

  def pbody(name, j)
    begin
      args = ["one", "two", "three"]
      Csp::Proc.current.choose {
        a = args.shift
        j.times do |i|
          puts "#{name} iteration #{i} arg #{a}"
          Csp::Proc.yield
        end
        Csp::Proc.current.backtrack if args.length > 0
      }
      Csp::Proc.current.backtrack
    rescue => msg
      puts "#{name} raised : #{msg}"
    end
  end

 # create some threads

 Csp::Proc.new { pbody("proc1", 3) }.resume
 Csp::Proc.new { pbody("proc2", 7) }.resume
 Csp::Proc.new { pbody("another thread", 6) }.resume

 #
 # Add a thread to check for termination condition
 # --  when there are no processes other than monitor
 #

 Csp::Proc.new {
   Csp::Proc.yield while (Csp::Proc.processes > 1)
   EM.stop
 }.resume
end

