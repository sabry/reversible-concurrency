require_relative 'csp'

Csp.SetTraceFile(Pathname.new(__FILE__).basename(".rb"))
Csp.Trace(1)

EM.synchrony do

  RUNS = 20

  def sender(cnt, c)
    cnt.times do |i|
      rand(30).times { Csp.yield }
      c.snd "msg:#{i} from #{Csp.name}:"
    end
  end

  def receiver(cnt,c1, c2)
    cnt.times do 
      Csp.alt [ 
               c1.g { |temp| puts "Receive on #{c1.name} #{temp}"},
               c2.g { |temp| puts "Receive on #{c2.name} #{temp}"} 
              ]
    end
  end

  Csp.proc("root",[],[]) {

    c1 = Csp.channel("c1")
    c2 = Csp.channel("c2")

    # create some threads

    Csp.proc("receiver",[c1,c2],[]) { receiver(RUNS * 2,c1, c2) }
    Csp.proc("sender 1",[],[c1]) { sender(RUNS, c1) }
    Csp.proc("sender 2",[],[c2]) { sender(RUNS, c2) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

