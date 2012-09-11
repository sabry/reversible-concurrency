require_relative 'csp'

EM.synchrony do

 #
 # code for a thread
 # just prints local time stamp and increments it
 #

  def pbody(name, j)
    begin
      args = ["one", "two", "three"]
      Csp::Proc.current.choose {
        a = args.shift
        j.times do |i|
          f = Csp::Proc.current
          puts "#{name} #{i} time = #{f.timestamp} arg #{a}"
          f.timestamp = f.timestamp + 1
          Csp::Proc.yield
        end
        Csp::Proc.current.backtrack if args.length > 0
      }
      Csp::Proc.current.backtrack
    rescue => msg
      puts "#{name} raised : #{msg}"
    end
  end

 # create some threads

 Csp::Proc.new { pbody("proc1", 3) }.resume
 Csp::Proc.new { pbody("proc2", 7) }.resume
 Csp::Proc.new { pbody("another thread", 6) }.resume

 #
 # Add a thread to check for termination condition
 # termination condition is when there are no other
 # remaining Csp processes
 #

 Csp::Proc.new {
   Csp::Proc.yield while (Csp::Proc.processes > 1)
   EM.stop
 }.resume
end

