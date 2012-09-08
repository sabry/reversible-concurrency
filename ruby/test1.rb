require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 # just prints local time stamp and increments it
 #

 def pbody(name, j)
   j.times do |i|
     f = Csp.current
     puts "#{name} #{i} time = #{f.timestamp}"
     f.timestamp = f.timestamp + 1
     Csp.yield
   end
 end

 # create some threads

 Csp.new { pbody("proc1", 3) }.resume
 Csp.new { pbody("proc2", 7) }.resume
 Csp.new { pbody("another thread", 6) }.resume

 #
 # Add a thread to check for termination condition
 # termination condition is when there are no other
 # remaining Csp processes
 #

 Csp.new {
   Csp.yield while (Csp.processes > 1)
   EM.stop
 }.resume
end

