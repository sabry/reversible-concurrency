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
      puts "Receive #{temp} at time #{Csp.time}"
    end
  end

  Csp.proc([],[]) {
    c = Csp.channel

    # create some threads

    Csp.proc([],[c]) { sender(5,c) }
    Csp.proc([c],[]) { receiver(5,c) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

