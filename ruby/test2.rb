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

  Csp.proc("root",[], []) {

    c = Csp.channel("c")

    # create some threads

    Csp.proc("sender", [], [c]) { sender(5,c) }
    Csp.proc("receiver", [c],[])  { receiver(5,c) }

    #
    # Add a thread to check for termination condition
    # termination condition is when there are no other
    # remaining Csp::Proc processes
    #

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
    }
end

