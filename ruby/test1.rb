require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 # just prints local time stamp and increments it
 #

 def pbody(j)
   j.times do |i|
     f = Csp::CspProc.current
     puts "#{Csp.name} #{i} time = #{Csp.time}"
     f.timestamp = Csp.time + 1
     Csp.yield
   end
 end

 # create some threads

 Csp.proc("root",[],[]){

   Csp.proc("proc1",[],[]) { pbody(3) }
   Csp.proc("proc2",[],[]) { pbody(7) }
   Csp.proc("proc3",[],[]) { pbody(6) }

   Csp.yield while (Csp::CspProc.processes > 1)
   EM.stop
 }
end

