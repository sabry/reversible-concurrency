require_relative 'csp'

EM.synchrony do
  
  def pp(a)
    s = ""
    a.each do |n|
      s += "#{n}"
    end
    puts s
  end

  def receiver(ch, cho)
    nums = Array.new
    max = 0
    
    ch.shuffle.each do |c|
      n = c.rcv
      nums.push n
      if n < max
        pp(nums)
        Csp.backtrack
      end
      max = n
    end
  
    pp(nums)

    #required to terminate
    cho.each do |c|
      c.snd(0)
    end
    
  end
  
  def sender(cout, cdone, n)
    cout.snd(n)

    #required to terminate
    Csp.yield until cdone.probe
    cdone.rcv
  end
  
  def setup(cnt)

    ch = Array.new cnt
    cho = Array.new cnt

    cnt.times do |i|

      c = Csp.channel("c#{i}")
      ch[i] = c

      cdone = Csp.channel("cdone#{i}")
      cho[i] = cdone

      Csp.proc("proc#{i}",[cdone],[c]) { sender(c, cdone, i) }
    end

    Csp.proc("master",ch,cho)  { receiver(ch, cho) }
  end
  
  
  Csp.proc("root",[],[]) {
    
    if ARGV[0] == nil
      puts "Give one command-line argument to specify number of items to sort."
      EM.stop
    else
      n = ARGV[0].to_i
      
      setup(n)
      
      Csp.yield while (Csp::CspProc.processes > 1)
      EM.stop
    end
  }
end

