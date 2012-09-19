require "em-synchrony"
require "continuation"

#
# to do --
#     move the "event" stuff out of channels, it doesn't
#     belong there.  Every time a process inherits or creates
#     a channel, add that to the current context
#     backtrack, exit should take care of cleaning up the context
#

module Csp

  # channel states

  SND_EV  = 0    # send on channel
  RCV_EV  = 1    # receive from channel
  PROC_EV = 2    # create a process
  CHAN_EV = 3    # create a channel
  
  # process states

  DEFUNCT = -1   # dead
  RUN     = 0        # normal mode
  BACK    = 1        # reversing

  class Channel

    REV  = -1
    IDLE = 0
    FWD  = 1

    attr_reader   :ack
    attr_reader   :req
    attr_reader   :name

    def initialize(name, f) 
      @ack = 0
      @req = 0
      @data = nil
      @txstate = IDLE
      @rxstate = IDLE
      @txint = false
      @rxint = false
      @name = name
    end

    def to_s
      "chan #{@name} ack=#{@ack} req=#{@req} txstate=#{@txstate}" +
        " rxstate=#{@rxstate} txint=#{@txint} rxint=#{@rxint}"
    end

    def snd(d)  
      f = CspProc.current
      raise "tx not idle" unless @txstate == IDLE
      raise "timestamp error" unless f.timestamp >= @req

      # do the handshake

      Csp.yield while (@rxstate == FWD) or (@txint)
      @data, @req, @txstate = d, f.timestamp + 1, FWD
      Csp.yield while (@rxstate == IDLE)
      @req, @txstate = @ack, IDLE
      f.timestamp = @req

    end

    def rcv
      f = CspProc.current
      raise "rx not idle" unless @rxstate == IDLE
      raise "timestamp error" unless f.timestamp >= @ack

      # do the handshake

      Csp.yield while @txstate == IDLE
      temp = @data
      @ack, @rxstate = [@req, f.timestamp + 1].max, FWD
      Csp.yield while (@txstate == FWD)
      @rxstate = IDLE
      f.timestamp = @ack

      # return received value

      return temp
    end

    def probe
      return @txstate == FWD
    end

    def g(&b)
      c = self
      return lambda {  
        break false if !c.probe
        b.call(c.rcv)
        break true }
    end

    def rev?
      return true if @rxint 
      return true if @txint  
      return true if (@rxstate == REV) 
      return true if (@txstate == REV)
      return false
    end

    def txrev(ts)
      if (@rxstate == IDLE) and (@txstate == IDLE) and (ts < @req)
        @txstate, @req = REV, ts
      end
      if (@rxstate == REV) and !(@txstate == IDLE)
        @txstate, @ack, @rxint = IDLE, @ack, false
      end
      if (@txstate == FWD) and (ts <  @req)
        @rxint = true 
      end
    end
    
    def rxrev(ts)
      if (ts < @ack) and (@txstate == IDLE) and (@rxstate == IDLE)
        @txint = true 
      end

      if (ts < @ack) and (@rxstate == IDLE) and !(@txstate == IDLE)
        @rxstate,@ack, @txint = REV, [ts, @req].min, false
      end

      @rxstate = IDLE if (@rxstate == REV) and (@txstate = IDLE)
    end
  end

  class CspProc < Fiber


    @@processes = 0

    attr_reader :state
    attr_reader :name
    attr_accessor :timestamp

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

      attr_reader :cont
      attr_reader :timestamp
      attr_reader :snd_port
      attr_reader :rcv_port

      # attr_reader :chans
      # attr_accessor :procs

      def initialize(cc,ts)
        @cont       = cc
        @timestamp  = ts
        @rcv_port  = Hash.new
        @snd_port  = Hash.new
        @name = "default"
      end


      def event(ts, obj, ev)
        case ev
        when SND_EV
          @snd_port[obj]  = ts
        when RCV_EV
          @rcv_port[obj]  = ts
        else
          raise "unexpected event!"
        end
      end

      def reap
        # this might be the place to
        # clean up -- still undecided
      end
    end



    def initialize(name, ts,cin,cout,&blk)
      @timestamp = ts
      @state = RUN
      @name = name
      
      super() {
        callcc {|cc|
          # set up initial context
          @cstack = [ Context.new(cc, @timestamp) ]
          cin.each {|c|  
            # c.receivers.push CspProc.current
            event(c.ack, c, RCV_EV)
          }
          cout.each {|c| 
            # c.senders.push CspProc.current
            event(c.req, c, SND_EV)
          }
          blk.call           # call process code
        }
        @state = DEFUNCT
        @@processes -= 1 
      }
      @@processes += 1
    end

    def to_s
      "proc #{@name} state=#{@state} time=#{@timestamp}"
    end


    def event(ts, chan, tp )
      @cstack.last.event(ts, chan, tp)
    end

    def choose(&blk)
      @timestamp += 1
      # create a new context
      callcc {|cc| 
        cout = @cstack.last.snd_port
        cin = @cstack.last.rcv_port
        push Context.new(cc, @timestamp)
        cout.each { |c,ts| event(c.req, c, SND_EV)}
        cin.each { |c,ts| event(c.ack, c, RCV_EV)} 
      }
      blk.call
      # destroy context
      oldcontext = pop
    end

    def backtrack

      raise "No saved context !" if (@cstack.length == 0)
      
      @state = BACK
      @timestamp = @cstack.last.timestamp
      Csp.yield while reverse!
      
      # call saved continuation

      @cstack.last.cont.call 

    end

    def push(c)
      @cstack.push c
    end

    def pop
      @cstack.pop
    end

    def reverse?
      # check all channels in context to see if 
      # reverse is requested

      cout = @cstack.last.snd_port
      cin = @cstack.last.rcv_port

      cout.each {|c,ts| 
        return true if c.rev?
      }
      cin.each {|c,ts| 
        return true if c.rev? 
      }
      return false
    end

    def reverse!
      cout = @cstack.last.snd_port
      cout.each {|c,ts| c.txrev(ts)}
      cin = @cstack.last.rcv_port
      cin.each {|c,ts| c.rxrev(ts) }
      
      if reverse?
        return true
      else
        @state = RUN
        return false
        end 
    end

    def self.processes 
      @@processes 
    end
    
    # think about how to clean up the dead
    # should probably check 

    #
    # Busy work should be state dependent.
    #   -- e.g. communicating should check for
    #      defunct children
    #      

    def yield
      f = self
      backtrack if (@state == RUN) and reverse?
      EM.next_tick {
        f.resume unless @state == DEFUNCT
      }
      Fiber.yield
    end

    def channel(name)
      Channel.new(name, self)
    end
  end

  #
  # Syntactic Sugar
  #

  def Csp.choose(&blk)
    CspProc.current.choose(&blk)
  end

  def Csp.backtrack
    CspProc.current.backtrack
  end

  def Csp.yield
    CspProc.current.yield
  end

  def Csp.proc(name, cin, cout, &blk)
    f = CspProc.current
    ts = f.is_a?(CspProc) ? f.timestamp : 0
    p = CspProc.new(name, ts,cin,cout) {blk.call}
    p.resume
    # put proc in parent's context
    #f.event(f.timestamp, p, PROC_EV) if f.is_a?(CspProc)
    return p
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
end
