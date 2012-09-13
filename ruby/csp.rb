require "em-synchrony"
require "continuation"

#
# to do --
#     move the "event" stuff out of channels, it doesn't
#     belong there.  Every time a process inherits or creates
#     a channel, add that to the current context
#     break should take care of that !
#

module Csp

  # constants used in this module

  Infinity =  1.0/0.0
  SND_EV = 0     # send on channel
  RCV_EV = 1     # receive from channel
  PROC_EV = 2    # create a process
  CHAN_EV = 3    # create a channel

  #
  # Context Class -- should this be local to Proc ?
  #


  #
  # Channel class
  #

  class Channel

    REV  = -1
    IDLE = 0
    FWD = 1

    attr_accessor :senders
    attr_accessor :receivers

    def initialize (f)
      @ack = 0
      @req = 0
      @data = nil
      @txstate = IDLE
      @rxstate = IDLE
      @txint = false
      @rxint = false
      f.event(f.timestamp, self, CHAN_EV) 
      @senders = [f]
      @receivers = [f]
    end

    def snd(d)  
      f = CspProc.current
      unless f == @senders.last
        raise "#{f} not current sender #{@senders.last}" 
      end
      raise "tx not idle" unless @txstate == IDLE
      Csp.yield while (@rxstate == FWD)

      # do the handshake

      @data, @req, @txstate = d, f.timestamp + 1, FWD
      Csp.yield while (@rxstate == IDLE)
      @req, @txstate = @ack, IDLE

      # save the event

      f.timestamp = @req
      f.event(@req, self, SND_EV)

    end

    def rcv
      f = CspProc.current
      raise "not current sender" unless f == @receivers.last
      raise "rx not idle" unless @rxstate == IDLE
      Csp.yield while @txstate == IDLE

      # do the handshake

      temp = @data
      @ack, @rxstate = [@req, f.timestamp + 1].max, FWD
      Csp.yield while (@txstate == FWD)
      @rxstate = IDLE
      f.timestamp = @ack

      # save the event

      f.event(@ack, self, RCV_EV)

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
  end

  # class variable processes keeps track of live Csp processes
  # timestamp used for local clock

  class CspProc < Fiber

    RUN = 0
    REV = 1
    DEAD = 2

    attr_accessor :timestamp
    @@processes = 0

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

      attr_reader :snd_event
      attr_reader :rcv_event
      attr_reader :chan_event
      attr_reader :proc_event
      attr_reader :cont
      attr_reader :timestamp

      def initialize(cc,ts)
        @cont       = cc
        @timestamp  = ts
        @rcv_event  = Hash.new(Infinity)
        @snd_event  = Hash.new(Infinity)
        @chan_event = Hash.new
        @proc_event = Hash.new
      end

      def event(ts, obj, ev)
        case ev
        when SND_EV
          @snd_event[obj] = [ts, @snd_event[obj]].min
        when RCV_EV
          @rcv_event[obj] = [ts, @rcv_event[obj]].min
        when PROC_EV
          @proc_event[obj] = ts
        when CHAN_EV
          @chan_event[obj] = ts
        else
          raise "unexpected event!"
        end
      end
    end

    def initialize(&blk)
      f = CspProc.current
      @timestamp = f.is_a?(CspProc) ? f.timestamp + 1 : 0
      @cstack = []
      f.event(f.timestamp, self, PROC_EV) if f.is_a?(CspProc)
      super {
        @state = RUN
        callcc {|cc|           
          @cstack.push Context.new(cc, @timestamp)
          blk.call
        }
        # need to release any channels and procs here
        @state = DEAD
        @@processes -= 1 
      }
      @@processes += 1
    end

    def event(ts, chan, tp )
      @cstack.last.event(ts, chan, tp) if @cstack.last
    end

    def choose(&blk)
      @timestamp += 1
      callcc {|cc| push Context.new(cc, @timestamp)}
      blk.call

      # under what conditions do we terminate ?
      # may need a list of "zombies" to reap
      # if we implement fork/join

      oldcontext = pop
      
      #
      # Need to release any channels and procs here
      #
    end

    def backtrack

      raise "No saved context !" if (@cstack.length == 0)
      
      # unwind events -- done in yield

      @state = REV
      Csp.yield 

      # Reset timestamp to start of context
      # call saved continuation

      @timestamp = @cstack.last.timestamp
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
      return false
    end

    def reverse!
      if (reverse?)
        backtrack if @STATE = FWD

         # run channels backwards
         # may need to call backtrack again !

      else
        @STATE = RUN
        return FALSE
      end

      # if we're done then go back to forward execution
      @state = RUN if @state = REV
    end


    def self.processes 
      @@processes 
    end

    def yield
      Fiber.yield if @state == DEAD
      begin
        f = self
        EM.next_tick {f.resume}
        Fiber.yield
      end while reverse! 
    end

    def channel
      c = Channel.new(self)
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

  def Csp.proc(cin, cout, &blk)
    p = CspProc.new(&blk)
    cin.each {|c|  c.receivers.push p}
    cout.each {|c| c.senders.push p}
    p.resume
  end

  def Csp.channel
    CspProc.current.channel
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
