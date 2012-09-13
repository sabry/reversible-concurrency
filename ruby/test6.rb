require_relative 'csp'

EM.synchrony do

  RUNS = 20

  def sender(name, cnt, c)
    cnt.times do |i|
      rand(30).times { Csp.yield }
      c.snd "msg:#{i} from #{name}:"
    end
  end

  def receiver(cnt,c1, c2)
    cnt.times do 
      Csp.alt [ 
                c1.g { |temp| puts "Receive on c1 #{temp}"},
                c2.g { |temp| puts "Receive on c2 #{temp}"} 
              ]
    end
  end


  Csp.proc([],[]) {

    c1 = Csp.channel
    c2 = Csp.channel

    # create some threads

    Csp.proc([c1,c2],[]) { receiver(RUNS * 2,c1, c2) }
    Csp.proc([],[c1]) { sender("sender 1", RUNS, c1) }
    Csp.proc([],[c2]) { sender("sender 2", RUNS, c2) }

    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

