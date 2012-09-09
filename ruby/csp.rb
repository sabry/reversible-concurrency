require 'em-synchrony'

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
  end

  def snd(d)  
    raise "tx not idle" if (@txstate != IDLE)
    raise "tx sending nil" if (d == nil)
    Csp.yield while (@rxstate == FWD)
    @data, @req, @txstate = d, Csp.current.timestamp + 1, FWD
    Csp.yield while (@rxstate == IDLE)
    @req, @txstate = @ack, IDLE
    Csp.current.timestamp = @req
  end

  def rcv(nb=false)
    raise "rx not idle" if (@rxstate != IDLE)
    return nil if nb & (@txstate != FWD)
    Csp.yield while @txstate == IDLE
    temp = @data
    @ack, @rxstate = [@req, Csp.current.timestamp + 1].max, FWD
    Csp.yield while (@txstate == FWD)
    @rxstate = IDLE
    Csp.current.timestamp = @ack
    return temp
  end
end

# class variable processes keeps track of live Csp processes
# timestamp used for local clock

class Csp < Fiber

  attr_accessor :timestamp
  @@processes = 0

  def initialize(&blk)
    @timestamp = 0
    super {
      blk.call
      @@processes -= 1 
    }
    @@processes += 1
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

