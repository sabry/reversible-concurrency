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

  c1 = Csp.channel
  c2 = Csp.channel

  # create some threads

  Csp.proc { receiver(RUNS * 2,c1, c2) }.resume
  Csp.proc { sender("sender 1", RUNS, c1) }.resume
  Csp.proc { sender("sender 2", RUNS, c2) }.resume

  #
  # Add a thread to check for termination condition
  # -- in this case,  when there are no other
  # remaining Csp::Proc processes
  #

  Csp.proc {
    Csp.yield while (Csp::Proc.processes > 1)
    EM.stop
  }.resume
end

