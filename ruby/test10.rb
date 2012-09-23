require_relative 'csp'

EM.synchrony do

  def p1(c12,c21)
    flag = true
    j = -1
    Csp.choose {
      if flag 
        c12.snd 1
        puts "!! p1 sending 1"
      else 
        c12.snd 2
        puts "!! p1 sending 2"
      end
      j = c21.rcv
      if j == 0
        flag = false
        puts "!! p1 received 0; backtracking"
        Csp.backtrack 
      end
    }
    puts "!!!!!!!!!! p1 result = #{j}"
    puts "!!!!!!!!!! p2 done !"
  end
  
  def p2(c12,c21)
    j = c12.rcv
    res = -1
    if (j == 1)
      puts "!! p2 received 1"
      puts "!! p2 sending 0"
      c21.snd 0
      res = 0
    else 
      puts "!! p2 received something other than 1"
      puts "!! p2 sending 1"
      c21.snd 1
      res = 10
    end
# without the next line p2 finishes and p1 blocks; BAD
    rand(30).times { Csp.yield } 
    puts "!!!!!!!!!! p2 result = #{res}"
    puts "!!!!!!!!!! p2 done !"
  end

  Csp.proc("root", [],[]) {

    c12 = Csp.channel("p1->p2")
    c21 = Csp.channel("p2->p1")

    # create some threads

    Csp.proc("p1",[c21],[c12]) { p1(c12,c21) } 
    Csp.proc("p2",[c12],[c21]) { p2(c12,c21) }
    Csp.yield while (Csp::CspProc.processes > 1)
    EM.stop
  }
end

