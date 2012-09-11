require_relative 'csp'

EM.synchrony do

  def sender(j,c)
    j.times do |i|
      c.snd(i)
      puts "Send #{i} at time #{Csp.time}"
    end
  end

  def receiver(j,c)
    j.times do |i|
      temp = c.rcv
      ts = Csp.time
      puts "Receive #{temp} at time #{ts}"
    end
  end

  c = Csp.channel

  # create some threads

  Csp.proc { sender(5,c) }.resume
  Csp.proc { receiver(5,c) }.resume

  #
  # Add a thread to check for termination condition
  # termination condition is when there are no other
  # remaining Csp::Proc processes
  #

  Csp.proc {
    Csp.yield while (Csp::Proc.processes > 1)
    EM.stop
  }.resume
end

