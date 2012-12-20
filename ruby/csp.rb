require "em-synchrony"
require "continuation"
require "pathname"

#
# to do --
#     move the "event" stuff out of channels, it doesn't
#     belong there.  Every time a process inherits or creates
#     a channel, add that to the current context
#     backtrack, exit should take care of cleaning up the context
#

module Csp

  @@traceno = 0
  @@tracefile = "csptrace"
  @@traceon = false

  def self.Location
    cstack = caller()
    loc = cstack.index{|x|
      not x.split(':')[0] == __FILE__
    }
    # want file location and method called in csp.rb
    floc = cstack[loc].split(':')
    ploc = cstack[loc-1].split(':')
    floc[2] = ploc[2];
    loc = floc.join(':')
    loc = File.basename(floc.join(':'))
  end

  def self.TraceFile

    if (@@tracefile.nil?)
      nil
    else
      @@traceno += 1
      [ '%s%03d.dot' % [@@tracefile, @@traceno-1], @@traceno-1]
    end
  end

  def self.SetTraceFile(fname)
    @@tracefile = fname
  end

  def self.Trace(flag)
    @@traceon = flag ? true : false
  end

  def Csp.log(ll)
    return false #1 < ll
  end

  # channel states

  SND_EV  = 0    # send on channel
  RCV_EV  = 1    # receive from channel
  PROC_EV = 2    # create a process
  CHAN_EV = 3    # create a channel
  
  # process states 

  KILL    = -2   # marked for death
  DEFUNCT = -1   # dead
  RUN     = 0    # normal mode
  BLOCK   = 1    # blocked on communication event
  BACK    = 2    # reversing

  class Channel

    REV  = -1
    IDLE = 0
    FWD  = 1

    @@nextid = 0
    attr_reader   :id
    attr_reader   :ack
    attr_reader   :req
    attr_reader   :name
    
    attr_accessor :sender     # stack of senders -- top is current
    attr_accessor :receiver   # stack of receivers -- top is current

    def initialize(name, f) 
      @ack = 0
      @req = 0
      @data = nil
      @txstate = IDLE
      @rxstate = IDLE
      @txint = false
      @rxint = false
      @sender = []
      @receiver = []
      @name = name
      @id = @@nextid
      @@nextid += 1
    end

    def to_s
      "chan #{@name} ack=#{@ack} req=#{@req} txstate=#{@txstate}" +
        " rxstate=#{@rxstate} txint=#{@txint} rxint=#{@rxint}"
    end

    def snd(d)  
      f = CspProc.current
      raise "${f.name} not current sender in #{@name}" unless f == self.sender.last
      raise "tx not idle in #{self}" unless @txstate == IDLE
      raise "timestamp error #{self} timestmp #{f.timestamp}" unless f.timestamp >= @req

      # do the handshake

      f.state = BLOCK

      Csp.yield while (@rxstate == FWD) or (@txint)
      @data, @req, @txstate = d, f.timestamp + 1, FWD

      Csp.yield while (@rxstate == IDLE)

      receiver = self.receiver.last
      @req, @txstate = @ack, IDLE

      # update process time, trace event, complete handshake

      f.sync(@req, "C#{@id}S#{@req}","C#{@id}R#{@req}", Csp.Location())


      Csp.yield while (@rxstate != IDLE)
      
      # update process state

      f.state = RUN

    end

    def rcv
      f = CspProc.current
      raise "${f.name} not current receiver in #{@name}" unless f == self.receiver.last
      raise "rx not idle #{@name}" unless @rxstate == IDLE
      raise "timestamp error #{@name}" unless f.timestamp >= @ack

      # do the handshake

      f.state = BLOCK

      Csp.yield while @txstate == IDLE
      sender = self.sender.last
      temp = @data
      @ack, @rxstate = [@req, f.timestamp + 1].max, FWD

      # update process state, trace event, complete handshake


      
      Csp.yield while (@txstate == FWD)

      f.sync(@ack, "C#{@id}R#{@ack}","C#{@id}S#{@ack}",Csp.Location())
      Csp.pdump
      @rxstate = IDLE

      #update process state

      f.state = RUN


      # return received value

      return temp
    end

    def probe
      return @txstate == FWD
    end

    #
    # create a guarded command
    #

    def g(&b)
      c = self
      return lambda {  
        break false if !c.probe
        b.call(c.rcv)
        break true 
      }
    end

    #
    # test if channel is being reversed
    #

    def rev?
      if @rxint or @txint or (@rxstate == REV) or (@txstate == REV)
        return true
      else
        return false
      end
    end

    #
    # transmitter reverses -- non-blocking, each call pushes the ball along
    #

    def txrev(ts)
      if (@rxstate == IDLE) and (@txstate == IDLE) and (ts < @req)
        puts "txrev #{self} #{ts}" if Csp.log(1)
        @txstate, @req = REV, ts
      end
      if (@rxstate == REV) and !(@txstate == IDLE)
        puts "txrev #{self} #{ts}" if Csp.log(1)
        @txstate, @req, @rxint = IDLE, @ack, false
      end
      if (@txstate == FWD) and (ts <  @req)
        puts "txrev #{self} #{ts}" if Csp.log(1)
        @rxint = true 
      end
    end

    #
    # receiver reverses -- non-blocking
    
    def rxrev(ts)
      if (ts < @ack) and (@txstate == IDLE) and (@rxstate == IDLE)
        puts "rxrev #{self} #{ts}" if Csp.log(1)
        @txint = true 
      end
      if (ts < @ack) and (@rxstate == IDLE) and !(@txstate == IDLE)
        puts "rxrev #{self} #{ts}" if Csp.log(1)
        @rxstate,@ack, @txint = REV, [ts, @req].min, false
      end
      if (@rxstate == REV) and (@txstate == IDLE)
        puts "rxrev #{self} #{ts}" if Csp.log(1)
        @rxstate = IDLE 
      end
    end
  end

  class CspProc < Fiber

    @@processes = 0
    @@nextid = 0
    @@root = nil

    attr_accessor :state
    attr_reader :name
    attr_accessor :timestamp
    attr_reader :id
    attr_reader :timstart

    #
    # process contexts -- includes
    #    continuation
    #    channel snd & rcv  events -- only
    #       the oldest event on a channel
    #       is relevant
    #    channels created
    #    processes created (children)
    #

    class Context

      attr_reader   :cont
      attr_reader   :timestamp
      attr_reader   :snd_port
      attr_reader   :rcv_port
      attr_accessor :chans
      attr_accessor :procs
      attr_accessor :trace
      attr_reader   :cin
      attr_reader   :cout

      def initialize(cc,ts)
        @cont       = cc
        @timestamp  = ts
        @rcv_port   = Hash.new
        @snd_port   = Hash.new
        @procs      = []
        @chans      = []
        @trace      = []
        @name = "default"
      end

      def event(ts, obj, ev)
        case ev
        when SND_EV
          @snd_port[obj]  = ts
        when RCV_EV
          @rcv_port[obj]  = ts
        when PROC_EV
          @procs.push [obj,ts]
        when CHAN_EV
          @chans.push obj
          obj.sender.push self
          obj.receiver.push self
        else
          raise "unexpected event!"
        end
      end

      # dump state nodes for trace

      def pnodes(ofile, indent)
        h = Hash.new()
        @trace.each { |t|
          if h[t[1]].nil?
#            t[3] = File.basename(t[3])
            location = t[3] ? " - " + t[3].split(':')[0..2].join(':')  : ""
            ofile.puts "#{indent}  #{t[1]} [label = \"#{t[0]} #{location}\"];"
            h[t[1]] = 1;
          end
        }
      end

      # dump transtions for trace

      def ptrans(ofile, indent, last)
        @trace.each { |t| 
          ofile.puts "#{indent} #{t[1]} -> #{t[2]};" unless t[2].nil? 
          unless last.nil? or (last == t[1])
            ofile.puts "#{indent} #{last} -> #{t[1]};"
          end
          last = t[1]
        }
        last
      end
        
    end

    def initialize(name, ts,cin,cout,srcloc,&blk)
      @timestamp = ts
      @timstart  = ts
      @state = RUN
      @name = name
      @cin = cin
      @cout = cout
      @id = @@nextid
      @@nextid += 1
      
      super() {
        callcc {|cc|
          # set up initial context

          @@root = self if @@root.nil?
          @cstack = [ Context.new(cc, @timestamp) ]
          @cstack.last.trace.push([@timestamp, "P#{@id}N#{@timestamp}", nil , srcloc])
          @cin.each {|c| 
            event(c.ack, c, RCV_EV) 
            c.receiver.push self 
          }
          @cout.each {|c| 
            event(c.req, c, SND_EV)
            c.sender.push self 
          }
        }
        blk.call           # call process code
        cleanup
      }
      @@processes += 1
    end

    def cleanup
        @cin.each {|c| c.receiver.pop
        puts "#{c} #{c.receiver.last}" if Csp.log(1)
      }
        @cout.each {|c| c.sender.pop
        puts "#{c} #{c.sender.last}" if Csp.log(1)
      }
        puts "#{self} defunct" if Csp.log(1)
        @state = DEFUNCT
        @@processes -= 1 
   end

    def to_s
      "proc #{@name} state=#{@state} time=#{@timestamp}"
    end

    def event(ts, obj, tp )
      @cstack.last.event(ts, obj, tp)
      if (tp == PROC_EV)
        evtrace = [ts,"P#{@id}N#{ts}","P#{obj.id}N#{obj.timstart}",nil,Csp.Location()]
        @cstack.last.trace.push(evtrace)
      end
    end

    def stable(&blk)
      @timestamp += 1
      # create a new context
      location = Csp.Location()
      callcc {|cc| 
        cout = @cstack.last.snd_port
        cin = @cstack.last.rcv_port
        @cstack.push Context.new(cc, @timestamp)
        @cstack.last.trace.push([@timestamp, "P#{@id}N#{@timestamp}", nil, location])
        cout.each { |c,ts| event(c.req, c, SND_EV)}
        cin.each { |c,ts| event(c.ack, c, RCV_EV)} 
      }
      blk.call
      # exit choose, destroy context
      oldcontext = @cstack.pop
    end

    def backtrack

      # no more contexts !
      
      if @cstack.length == 0
        cleanup
        Csp.yield
      end

      raise "No saved context !" if (@cstack.length == 0)

      puts "#{self} backtracking" if Csp.log(1)

      @state = BACK
      @timestamp = @cstack.last.timestamp

      #puts "#{@name} children = #{@cstack.last.procs}"

      # kill all our children
      
      @cstack.last.procs.each {|p| p[0].kill unless p[0].state == DEFUNCT}
      @cstack.last.procs.each {|p| Csp.yield unless p[0].state == DEFUNCT}
      @cstack.last.procs = []


      # release all channels created

      @cstack.last.chans = []

      # run backwards
      
      while reverse!
        puts "#{self} in backtrack" if Csp.log(1)
        Csp.yield 
      end

      # erase trace history
      
      ts = @cstack.last.timestamp
      @cstack.last.trace = [@cstack.last.trace[0]] #[[ts, "P#{@id}N#{ts}"]]

      puts "#{self} exiting backtrack" if Csp.log(1)
      
      # call saved continuation

      @cstack.last.cont.call 
    end

    def reverse?

      # check all channels in context to see if 
      # reverse is requested

      cout = @cstack.last.snd_port
      cin = @cstack.last.rcv_port

      cout.each {|c,ts| return true if (c.sender.last == self) and c.rev?}
      cin.each {|c,ts|  return true if (c.receiver.last == self) and c.rev? }
      return false
    end

    def reverse!
      
      # move things backward

      @cstack.last.snd_port.each {|c,ts| 
        if c.sender.last == self
          if c.ack < ts
            puts "#{self} popping context ts = #{ts}" if Csp.log(1)
            cs = @cstack.pop
            backtrack
          else
            c.txrev(ts) 
          end
        end
      }

      @cstack.last.rcv_port.each {|c,ts| 
        if c.receiver.last == self
          if c.req < ts
            puts "#{self} popping context ts = #{ts}" if Csp.log(1)
            @cstack.pop
            backtrack
          else
            c.rxrev(ts)
          end
        end
      }

      if reverse?
        return true
      else
        puts "#{self} returning to run" if Csp.log(1);
        @state = RUN
        return false
      end 
    end

    def sync(ts, from, to, c)
      @timestamp = ts
      @cstack.last.trace.push([ts, from, to, c]);
    end

    def self.processes 
      @@processes 
    end

    def self.root
      @@root
    end
    
    def yield
      backtrack if ((@state == RUN) or (@state == BLOCK)) and reverse?
      f = self
      EM.next_tick {f.resume unless @state == DEFUNCT }
      Fiber.yield
    end

    def kill
      #puts "#{@name} being killed"
      while oldcontext = @cstack.pop
        oldcontext.procs.each {|p| p[0].kill if p[0].state != DEFUNCT }
        oldcontext.procs.each {|p| yield if p[0].state != DEFUNCT  }
      end
      cleanup
    end

    def channel(name)
      Channel.new(name, self)
    end

    def tick()
      @timestamp += 1
      @cstack.last.trace.push([@timestamp, "P#{@id}N#{@timestamp}", nil, Csp.Location()]) 
      Csp.pdump
    end

    # print transitions from trace

    def ptrans(ofile, indent)
      last = nil
      @cstack.each { |ctxt| last = ctxt.ptrans(ofile, indent, last) }
    end

    # print process graph

    def pgraph(ofile)
      def pcluster(ofile, cs, indent)
        unless cs.empty?
          statename =  ["kill", "DEFUNCT", "RUN", "BLOCK", "BACK"][@state + 2]
          ctxt = cs.shift
          ts = ctxt.timestamp
          nm =  (ts == @timstart) ? "#{@name},#{ts},#{statename}" : "#{ts}"
          ofile.puts "#{indent}subgraph cluster_p#{@id}_#{ts} {"
          ofile.puts "#{indent}  label = \"#{nm}\";"
          ofile.puts "#{indent}  labeljust = \"right\";"
          ofile.puts "#{indent}  color = blue"
          pcluster(ofile, cs, "#{indent}  ")
          ctxt.pnodes(ofile, indent);
          ofile.puts "#{indent} }"
        end
      end

        
      pcluster(ofile, Array.new(@cstack), "  ")
      @cstack.each { |c| c.procs.each { |p| p[0].pgraph(ofile) }}
      @cstack.each { |c| c.procs.each { |pc| pc[0].ptrans(ofile,"  ")}}
      ptrans(ofile, "  ");
    end
  end

  #
  # Syntactic Sugar
  #

  def Csp.par(proclist)
    proclist.each {|p| Csp.yield while (p.state != DEFUNCT) }
  end

  def Csp.stable(&blk)
    CspProc.current.stable(&blk)
  end

  def Csp.backtrack
    CspProc.current.backtrack
  end

  def Csp.yield
    CspProc.current.yield
  end

  def Csp.proc(name, cin, cout, &blk)
    srcloc = blk.source_location[0]  + ":#{blk.source_location[1]}"
    f = CspProc.current
    ts = f.is_a?(CspProc) ? f.timestamp + 1 : 0
    p = CspProc.new(name, ts,cin,cout,srcloc) {blk.call}
    p.resume
    # put proc in parent's context
    f.event(f.timestamp, p, PROC_EV) if f.is_a?(CspProc)
    Csp.pdump
    return p
  end

  def Csp.tick()
    f = Csp.current
    f.tick() if f.is_a?(CspProc)
  end

  def Csp.channel(name)
    CspProc.current.channel(name)
  end

  def Csp.alt(a)
    while a.shuffle.each { |b| break false if b.call } do
      Csp.yield
    end
  end

  def Csp.time
    CspProc.current.timestamp
  end

  def Csp.name
    CspProc.current.name
  end

  def Csp.current
    CspProc.current
  end

  def Csp.pdump(p=nil)
    if @@traceon
      if (p)
        p.pgraph(ofile)
      else
        filename = Csp.TraceFile()
        unless filename.nil?
          ofile = File.open(filename[0], "w")
          location = Csp.Location()
          ofile.puts "strict digraph G {"
          ofile.puts "  concentrate=true;"
          ofile.puts "  page=\"8.5,11\";"
          ofile.puts "  size=\"8,10\";"
          ofile.puts "  labelloc=\"top\";"
          ofile.puts "  label=\"#{filename[0]}\";"
          ofile.puts "  orientation=\"landscape\";"
          ofile.puts "  fontsize=\"10\";"
          CspProc.root.pgraph(ofile)
          ofile.puts "}"
          ofile.close
        end
      end
    end
  end
end
