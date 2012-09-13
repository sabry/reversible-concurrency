require "em-synchrony"
require "continuation"

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

    def initialize
      @ack = 0
      @req = 0
      @data = nil
      @txstate = IDLE
      @rxstate = IDLE
      @txint = false
      @rxint = false
      f = Proc.current
      f.event(f.timestamp, self, CHAN_EV) if f.is_a?(Proc)
    end

    def snd(d)  
      raise "tx not idle" if (@txstate != IDLE)
      Csp.yield while (@rxstate == FWD)

      # do the handshake

      @data, @req, @txstate = d, Proc.current.timestamp + 1, FWD
      Csp.yield while (@rxstate == IDLE)
      @req, @txstate = @ack, IDLE

      # save the event

      Proc.current.timestamp = @req
      Proc.current.event(@req, self, SND_EV)

    end

    def rcv
      raise "rx not idle" if (@rxstate != IDLE)
      Csp.yield while @txstate == IDLE

      # do the handshake

      temp = @data
      @ack, @rxstate = [@req, Proc.current.timestamp + 1].max, FWD
      Csp.yield while (@txstate == FWD)
      @rxstate = IDLE
      Proc.current.timestamp = @ack

      # save the event

      Proc.current.event(@ack, self, RCV_EV)

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

  class Proc < Fiber

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
      @timestamp = 0
      @cstack = []
      f = Proc.current
      f.event(f.timestamp, self, PROC_EV) if f.is_a?(Proc)
      super {
        @state = RUN
        blk.call
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

      # merge events from popped context

      ev = oldcontext.rcv_event
      ev.keys.each {|k| event(ev[k], k, RCV_EV)}
      ev = oldcontext.snd_event
      ev.keys.each {|k| event(ev[k], k, SND_EV)}
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
  end

  #
  # Syntactic Sugar
  #

  def Csp.choose(&blk)
    Proc.current.choose(&blk)
  end

  def Csp.backtrack
    Proc.current.backtrack
  end

  def Csp.yield
    Proc.current.yield
  end

  def Csp.proc(&blk)
    Proc.new(&blk)
  end

  def Csp.channel
    Channel.new
  end

  def Csp.alt(a)
    while a.shuffle.each { |b| break false if b.call } do
      Csp.yield
    end
  end

  def Csp.time
    Proc.current.timestamp
  end
end
