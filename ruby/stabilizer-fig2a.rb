require_relative 'csp'

EM.synchrony do

  def t1 (c,c2) 
    count = 2
    puts "Entering t1"
    Csp.stable {
      puts "t1 entering stable region I"
      c.snd 0
      puts "t1 sent first message to t2"
      Csp.stable {
        puts "t1 entering stable region II"
        c.snd 0
        puts "t1 sent second message to t2"
        count = count - 1
        Csp.backtrack if count > 0
      }
    }
    c2.snd 0
    puts "t1 sent third message to t2"
    puts "t1 finishing..."
  end

  def t2 (c,c2) 
    puts "Entering t2"
    Csp.stable {
      puts "t2 entering stable region"
      c.rcv
      puts "t2 received first message from t1"
      c.rcv
      puts "t2 received first message from t1"
      c2.rcv
      puts "t2 received final message from t1"
    }
    puts "t2 finishing..."
  end

  Csp.proc("root", [],[]) {

    c  = Csp.channel("1->2")
    c2 = Csp.channel("1->2 term")

    Csp.par [
        Csp.proc("t1", [], [c,c2]) { t1(c,c2) },
        Csp.proc("t2", [c,c2], []) { t2(c,c2) } 
     ]
    EM.stop
  }
end
