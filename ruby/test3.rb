require_relative 'csp'

EM.synchrony do

  def sender(j,c)
    j.times do |i|
      c.snd(i)
      puts "Send #{i} at time #{Csp.current.timestamp}"
    end
  end

  def receiver(j,c)
    j.times do |i|
      #Csp.yield while (!c.probe)
      temp = c.rcv()
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

