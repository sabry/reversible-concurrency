require_relative 'csp'

EM.synchrony do
  
  def p3(c23,c34)
    puts "#{Csp.name} sending 4 on #{c34.name}"
    c34.snd 4
    v = c23.rcv
    puts "#{Csp.name} received #{v} on #{c23.name}"
    c34.snd v
    puts "#{Csp.name} terminating!"
  end

  def p4(c34,c42)
    v = c34.rcv
    puts "#{Csp.name} received #{v}  on #{c34.name}"
    c42.snd v
    c34.rcv
    puts "#{Csp.name} terminating!"
  end

  def p1(c12,c23,c42)
    vals =  [1, 2, 3, 4]
    Csp.stable {
      c34 = Csp.channel("3->4");
      v = vals.shift
      puts "#{Csp.name} sending #{v} on #{c12.name}"
      c12.snd v
      Csp.par [ 
          Csp.proc("p3", [c23],[c34]) { p3(c23,c34) },
          Csp.proc("p4", [c34],[c42]) { p4(c34,c42) }
        ]
    }
    puts "#{Csp.name} terminating!"
  end

  def p2(c12,c23,c42)
    Csp.stable {
      v1 = c12.rcv
      puts "#{Csp.name} received #{v1}  on #{c12.name}"
      v2 = c42.rcv
      puts "#{Csp.name} received #{v2}  on #{c42.name}"
      unless (v1 == v2)
        puts "#{Csp.name} backtracking!"
        Csp.backtrack 
      end
      puts "#{Csp.name} sending #{v1}  on #{c23.name}"
      c23.snd v1
    }
    puts "#{Csp.name} terminating!"
  end

  Csp.proc("root", [],[]) {

    c12 = Csp.channel("1->2")
    c23 = Csp.channel("2->3")
    c42 = Csp.channel("4->2")

    Csp.par [
        Csp.proc("p1", [c23], [c12,c42]) { p1(c12,c23,c42) },
        Csp.proc("p2", [c12,c42], [c23]) { p2(c12,c23,c42) }
     ]
    EM.stop
  }
end

