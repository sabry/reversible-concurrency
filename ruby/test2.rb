require_relative 'csp'

EM.synchrony do
  def sender(j,c)
    j.times do |i|
      c.send(i)
      puts "Send #{i} at time #{Csp.current.timestamp}"
    end
  end

  def receiver(j,c)
    j.times do |i|
      temp = nil
      temp = c.receive() while (temp == nil)
      ts = Csp.current.timestamp
      puts "Receive #{temp} at time #{ts}"
    end
  end

 c = Channel.new

 # create some threads

 Csp.new { sender(5,c) }.resume
 Csp.new { receiver(5,c) }.resume

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

