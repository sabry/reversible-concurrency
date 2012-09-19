require_relative 'csp'

EM.synchrony do

  RUNS = 20

  def sender(name, cnt, c)
    Csp.choose {
      cnt.times do |i|
        rand(30).times { Csp.yield }
        c.snd "msg:#{i} from #{name}:"
      end
    }
  end

  def receiver(cnt,c1, c2)
    Csp.choose {
      cnt.times do 
        Csp.alt [ 
            c1.g { |temp| puts "Receive on c1 #{temp}"},
            c2.g { |temp| puts "Receive on c2 #{temp}"} 
              ]
      end
    }
  end

  Csp.proc("root",[],[]) {

    c1 = Csp.channel("c1")
    c2 = Csp.channel("c2")

    # create some threads

    Csp.proc("receiver",[c1,c2],[]) { receiver(RUNS * 2,c1, c2) }
    Csp.proc("sender 1",[],[c1]) { sender("sender 1", RUNS, c1) }
    Csp.proc("sender 2",[],[c2]) { sender("sender 2", RUNS, c2) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

