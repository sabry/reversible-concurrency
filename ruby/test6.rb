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
      Channel.alt([ 
                    c1.g { |temp| puts "Receive on c1 #{temp}"},
                    c2.g { |temp| puts "Receive on c2 #{temp}"} 
                  ])
    end
  end

  c1 = Channel.new
  c2 = Channel.new

  # create some threads

  Csp.new { receiver(RUNS * 2,c1, c2) }.resume
  Csp.new { sender("sender 1", RUNS, c1) }.resume
  Csp.new { sender("sender 2", RUNS, c2) }.resume

  #
  # Add a thread to check for termination condition
  # termination condition is when there are no other
  # remaining Csp processes
  #

  Csp.new {
    Csp.yield while (Csp.processes > 1)
    EM.stop
  }.resume
end

