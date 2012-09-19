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
      Csp.choose {
        a = args.shift
        j.times do |i|
          puts "#{name} iteration #{i} arg #{a}"
          Csp.yield
        end
        Csp.backtrack if args.length > 0
      }
      Csp.backtrack
    rescue => msg
      puts "#{name} raised : #{msg}"
    end
  end

 # create some threads

 Csp.proc("root",[],[]) {
    Csp.proc("p1",[],[]) { pbody("proc1", 3) }
    Csp.proc("p2",[],[]) { pbody("proc2", 7) }
    Csp.proc("p3",[],[]) { pbody("another thread", 6) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

