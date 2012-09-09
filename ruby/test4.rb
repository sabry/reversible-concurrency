require_relative 'csp'

EM.synchrony do

  def sender(j,c1,c2)
    j.times do |i|
      if (i % 2) != 0
        c2.snd(i)
        puts "Send #{i} on c2 at time #{Csp.current.timestamp}"
      else
        c1.snd(i)
        puts "Send #{i} on c1 at time #{Csp.current.timestamp}"
      end
    end
  end

  def receiver(j,c1, c2)
    cnt = 0;
    while (1)
      case
        when temp = c1.rcv(true) then
          cnt += 1
          ts = Csp.current.timestamp
          puts "Receive from c1 #{temp} at time #{ts}"
        when temp = c2.rcv(true) then
          cnt += 1
          ts = Csp.current.timestamp
          puts "Receive from c2 #{temp} at time #{ts}"
        else 
          Csp.yield
        end 
        break if cnt == j
     end
   end

  c1 = Channel.new
  c2 = Channel.new

  # create some threads

  Csp.new { sender(5,c1, c2) }.resume
  Csp.new { receiver(5,c1, c2) }.resume

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

