require_relative 'csp'

Csp.SetTraceFile(Pathname.new(__FILE__).basename(".rb"))
Csp.Trace(1)

EM.synchrony do

  RUNS = 6

  def sender(name, cnt, c)
    cnt.times do |i|
      c.snd i
      puts "!! send #{i}"
    end
    puts "!!!!!!!!!! sender done !"
  end
  
  def receiver(cnt,c)
    i = 0
    j = 0
    Csp.stable {
      while 1
        j = c.rcv
        puts "!! receive #{j}"
        break if j == (cnt - 1)
        if (j == i)
          i += 1
          puts "!! backtrack i = #{i}"
          Csp.backtrack
        end
      end
    }
    puts "!!!!!!!!!! receiver done !"
  end

  Csp.proc("root", [],[]) {

    c1 = Csp.channel("c1")
    c2 = Csp.channel("c2")

    # create some threads

    Csp.proc("rcv", [c1],[]) { receiver(RUNS,c1) }
    Csp.proc("snd", [],[c1]) { sender("sender 1", RUNS, c1) }
    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

