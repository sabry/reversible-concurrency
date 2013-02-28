require_relative 'csp'

EM.synchrony do
  
  def pp(a)
    s = ""
    a.each do |n|
      s += "#{n}"
    end
    puts s
  end

  def receiver(ch, cho, verbose)
    
    # t var and stable point added to count number of backtracks
    t = 1;
    Csp.stable{
 
      nums = Array.new
      max = 0
      
      ch.shuffle.each do |c|
        n = c.rcv
        nums.push n
        if n < max
          pp(nums) if verbose
          t += 1;
          Csp.backtrack
        end
        max = n
      end
      pp(nums) if verbose
    }

    #required to terminate
    cho.each do |c|
      c.snd(0)
    end

    return t
    
  end
  
  def sender(cout, cdone, n)

    cout.snd(n)

    #required to terminate
    Csp.yield until cdone.probe
    cdone.rcv
  end
  
  def setup(cnt, verbose)

    ch = Array.new cnt
    cho = Array.new cnt

    cnt.times do |i|

      c = Csp.channel("c#{i}")
      ch[i] = c

      cdone = Csp.channel("cdone#{i}")
      cho[i] = cdone

      Csp.proc("proc#{i}",[cdone],[c]) { sender(c, cdone, i) }
    end
    t=0
    Csp.proc("master",ch,cho)  { t = receiver(ch, cho, verbose) }
    Csp.yield while (Csp::CspProc.processes > 1)
    return t
  end
  
  Csp.proc("root",[],[]) {
    
    # Check for command-line argument
    if ARGV[0] == nil
      puts "Give one command-line argument to specify number of items to sort."
      EM.stop
    else
      n = ARGV[0].to_i
      
      if ARGV[1] == nil
        setup(n, true)
      else
        cnt = ARGV[1].to_i
        tot = 0;
        for i in 1..cnt
          tot += setup(n, false)
          end
        puts (tot*1.0)/cnt
      end
      
      EM.stop
    end
  }
end

