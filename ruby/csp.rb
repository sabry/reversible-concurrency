require "em-synchrony"
require "continuation"

module Csp

  #
  # CEvent class
  #

  class CEvent
    SND = 0
    RCV  = 1

    attr_reader :dir
    attr_reader :channel
    attr_reader :timestamp

    def initialize(ts, chan, tp)
      @dir = tp
      @channel = chan
      @timestamp = ts
    end
  end

  #
  # Context Class
  #

  class Context
    attr_reader :events
    attr_reader :cont
    attr_reader :timestamp

    def initialize(cc,ts)
      @events = []
      @cont = cc
      @timestamp = ts
    end

    def push(e)
      events.push e
    end

    def pop
      events.pop
    end
  end

  #
  # Channel class
  #

  def Csp.alt(a)
    while a.shuffle.each { |b| break false if b.call } do
      Proc.yield
    end
  end


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
    end

    def snd(d)  
      raise "tx not idle" if (@txstate != IDLE)
      Proc.yield while (@rxstate == FWD)

      # do the handshake

      @data, @req, @txstate = d, Proc.current.timestamp + 1, FWD
      Proc.yield while (@rxstate == IDLE)
      @req, @txstate = @ack, IDLE

      # save the event

      Proc.current.timestamp = @req
      Proc.current.event(@req, self, CEvent::SND)

    end

    def rcv
      raise "rx not idle" if (@rxstate != IDLE)
      Proc.yield while @txstate == IDLE

      # do the handshake

      temp = @data
      @ack, @rxstate = [@req, Proc.current.timestamp + 1].max, FWD
      Proc.yield while (@txstate == FWD)
      @rxstate = IDLE
      Proc.current.timestamp = @ack

      # save the event

      Proc.current.event(@ack, self, CEvent::RCV)

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

    attr_accessor :timestamp
    @@processes = 0

    def initialize(&blk)
      @timestamp = 0
      @cstack = []
      super {
        blk.call
        @@processes -= 1 
      }
      @@processes += 1
    end

    def event(ts, chan, tp )
      @cstack.last.push CEvent.new(ts, chan, tp) if @cstack.last
    end

    def choose(&blk)
      @timestamp += 1
      callcc {|cc| push Context.new(cc, @timestamp)}
      blk.call
      # may need to unwind events -- how do we get here ?
      # under what conditions do we terminate ?
      # may need a list of "zombies" to reap
      # puts "context stack is #{@cstack.length} deep"
      pop
    end

    def backtrack
      # Need to unwind channel events
      @cstack.last.cont.call if @cstack.last
      raise "No saved context !"
    end

    def push(c)
      @cstack.push c
    end

    def pop
      @cstack.pop
    end

    def self.processes 
      @@processes 
    end

    def self.yield
      f = current
      EM.next_tick { f.resume }
      super
    end
  end

end
