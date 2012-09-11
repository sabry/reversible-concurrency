require_relative 'csp'

EM.synchrony do

  def sender(j,c1,c2)
    a = [
      (lambda { |i|
        c2.snd(i)
        puts "Send #{i} on c2 at time #{Csp::Proc.current.timestamp}"
      }),
      (lambda { |i|
        c1.snd(i)
        puts "Send #{i} on c1 at time #{Csp::Proc.current.timestamp}"
        })]

    j.times do |i|
      a.shuffle[0].call(i)
      end
  end

  def receiver(j,c1, c2)
    cnt = 0;
    while (1)
      case
        when c1.probe
          temp = c1.rcv
          cnt += 1
          ts = Csp::Proc.current.timestamp
          puts "Receive from c1 #{temp} at time #{ts}"
        when c2.probe
          temp = c2.rcv
          cnt += 1
          ts = Csp::Proc.current.timestamp
          puts "Receive from c2 #{temp} at time #{ts}"
        else 
          Csp::Proc.yield
        end 
        break if cnt == j
     end
   end

  c1 = Csp::Channel.new
  c2 = Csp::Channel.new

  # create some threads

  Csp::Proc.new { sender(5,c1, c2) }.resume
  Csp::Proc.new { receiver(5,c1, c2) }.resume

  #
  # Add a thread to check for termination condition
  # termination condition is when there are no other
  # remaining Csp::Proc processes
  #

  Csp::Proc.new {
    Csp::Proc.yield while (Csp::Proc.processes > 1)
    EM.stop
  }.resume
end

