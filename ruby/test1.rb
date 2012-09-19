require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 # just prints local time stamp and increments it
 #

 def pbody(name, j)
   j.times do |i|
     f = Csp::CspProc.current
     puts "#{name} #{i} time = #{Csp.time}"
     f.timestamp = Csp.time + 1
     Csp.yield
   end
 end

 # create some threads

 Csp.proc("root",[],[]){

   Csp.proc("proc1",[], []) { pbody("proc1", 3) }
   Csp.proc("proc2",[],[]) { pbody("proc2", 7) }
   Csp.proc("proc3",[],[]) { pbody("another thread", 6) }

   Csp.yield while (Csp::CspProc.processes > 1)
   EM.stop
 }
end

