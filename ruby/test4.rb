require_relative 'csp'

EM.synchrony do

  def sender(j,c1,c2)
    j.times do |i|
      if (i % 2) != 0
        c2.snd(i)
        puts "Send #{i} on c2 at time #{Csp.time}"
      else
        c1.snd(i)
        puts "Send #{i} on c1 at time #{Csp.time}"
      end
    end
  end

  def receiver(j,c1, c2)
    cnt = 0;
    while (1)
      case
        when c1.probe
          temp = c1.rcv
          cnt += 1
          puts "Receive from c1 #{temp} at time #{Csp.time}"
        when c2.probe
          temp = c2.rcv
          cnt += 1
          puts "Receive from c2 #{temp} at time #{Csp.time}"
        else 
          Csp.yield
        end 
        break if cnt == j
     end
   end

  Csp.proc("root",[],[]) {
    c1 = Csp.channel("c1")
    c2 = Csp.channel("c2")

    # create some threads

    Csp.proc("sender",[],[c1,c2]) { sender(5,c1, c2) }
    Csp.proc("receiver",[c1,c2],[]) { receiver(5,c1, c2) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

