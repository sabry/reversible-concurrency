require_relative 'csp'

EM.synchrony do

  def sender(j,c)
    j.times do |i|
      c.snd(i)
      puts "Send #{i} at time #{Csp::Proc.current.timestamp}"
    end
  end

  def receiver(j,c)
    j.times do |i|
      temp = c.rcv
      ts = Csp::Proc.current.timestamp
      puts "Receive #{temp} at time #{ts}"
    end
  end

  c = Csp::Channel.new

  # create some threads

  Csp::Proc.new { sender(5,c) }.resume
  Csp::Proc.new { receiver(5,c) }.resume

  #
  # Add a thread to check for termination condition
  # termination condition is when there are no other
  # remaining Csp::Proc processes
  #

  Csp::Proc.new {
    Csp::Proc.yield while (Csp::Proc.processes > 1)
    EM.stop
  }.resume
end

