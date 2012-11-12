require_relative 'csp'

EM.synchrony do

  def t1 (c) 
    puts "Entering t1"
    Csp.stable {
      puts "t1 entering stable region I"
      c.snd 0
      puts "t1 sent first message to t2"
      Csp.stable {
        puts "t1 entering stable region II"
        c.snd 0
        puts "t1 sent second message to t2"
        Csp.backtrack
      }
    }
    puts "t1 finishing..."
  end

  def t2 (c) 
    puts "Entering t2"
    Csp.stable {
      puts "t2 entering stable region"
      c.rcv
      puts "t2 received first message from t1"
      c.rcv
      puts "t2 received first message from t1"
    }
    puts "t2 finishing..."
  end

  Csp.proc("root", [],[]) {

    c = Csp.channel("1->2")

    Csp.par [
        Csp.proc("t1", [], [c]) { t1(c) },
        Csp.proc("t2", [c], []) { t2(c) } 
     ]
    EM.stop
  }
end
