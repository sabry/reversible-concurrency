require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 #
 #  Inside "stable"
 #      prints iteration and arg shifted in after stable
 #      increments timestamp
 # 
 #      after j iterations, backtracks
 #
 #

  def pbody(j)
    begin
      args = ["one", "two", "three"]
      Csp.stable {
        a = args.shift
        j.times do |i|
          puts "#{Csp.name} iteration #{i} arg #{a}"
          Csp.yield
        end
        Csp.backtrack if args.length > 0
      }
    rescue => msg
      puts "#{Csp.name} raised : #{msg}"
    end
  end

 # create some threads

 Csp.proc("root",[],[]) {
    Csp.proc("p1",[],[]) { pbody(3) }
    Csp.proc("p2",[],[]) { pbody(7) }
    Csp.proc("p3",[],[]) { pbody(6) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

