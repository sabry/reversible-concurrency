
package reversibleconcurrency5

import akka.actor.{ ActorRef, Actor, ActorSystem, Props }
import scala.util.continuations._
import scala.concurrent.stm._
import scala.concurrent.Promise
import scala.util.DynamicVariable

// Five.scala : 

// Going to try adding some limited, single channel, message
// backtracking functionality.

class ProcessActor(task: ReversibleContext => Unit) extends Actor {
  
  // config
  import context._
  val tracing = false // set to true to print all received messages

  // avoid hmap problems by representing the state as a stored partial
  // function of how to handle a message

  implicit object Context extends ReversibleContext {

    var sendClause: Ref.View[PartialFunction[Any,Unit]] =
      Ref( { case m => println(m) }: PartialFunction[Any,Unit] ).single
    
    var receiveClause: Ref.View[PartialFunction[Any,Unit]] =
      Ref( { case m => self ! m }: PartialFunction[Any,Unit] ).single

    val selfRef = self

    var chan:ChannelView[Int] = null

  }

  val errorClause: PartialFunction[Any,Unit] =
    { case m => println("Received an erronous message: " + m) }

  val tracer: PartialFunction[Any,Any] =
    { case m => if(tracing) println("received: " + m); m }

  def receive = tracer andThen {
    case m: Ack => Context.sendClause()(m)
    case m: Message[Any] => Context.receiveClause()(m)
    case m => errorClause(m)
  }

  task.apply(Context)
  
}
    
case class StableTask[In,Out](fun: (In,ReversibleContext) => Out @cpsParam[Out,Unit]) {
  var currentInput: In = null.asInstanceOf[In]
  def apply(input: In)(implicit context: ReversibleContext): Out @cpsParam[Out,Unit] = {
    currentInput = input
    fun(input, context)
  }
}

case class Backtrack[In](fun: (In,Backtrack[In])=>Any, startTime: Int, origInput: In) {
  def apply(input: In)(implicit context: ReversibleContext): Any = {
    // handle backtracking messages before calling continuation
    if(context.chan.time > startTime) {
      context.chan.timeDirection = Backwards
      context.chan.time = startTime
    }
    fun(input,this)
  }
  def apply(implicit context: ReversibleContext): Any = {
    apply(origInput)
  }
}


object Functions {

  // This is a functor parameterized by the system, but I need to
  // figure out the appropriate syntax. (aka I don't want this
  // import!)
  import TestSystem.system._

  def stable[In, Out](work: (In,Backtrack[In]) => Out): StableTask[In,Out] = {

    new StableTask[In,Out](
      (input: In, context: ReversibleContext) => {
        shift { k: (Out=>Any) =>          
          val bt = 
            Backtrack[In]( 
              (input: In, bt: Backtrack[In]) => k(work(input,bt)),
              context.time, input)
          work(input,bt)
        }
      }: Out @cpsParam[Out,Unit]
    )
    
  }
        
  def send[T](chan: Channel[T], msg: T)(implicit context: ReversibleContext): Unit @cpsParam[Unit,Unit] = {
    context.time+=1
    chan.receiver.future.map { 
      ref => ref ! Message(msg, chan.id, context.time)
    }
    shift { k: (Unit=>Unit) =>
      println("Sending message... " + msg)
      context.sendClause() = {
        case m @ Ack(chanId) if chanId == chan.id =>
          k.apply() 
        case m @ Ack(chanId) =>
          println("Error: a sending process received an erronous Ack: " + m)
      }
    }                               
  }  
 
  def receive[T](chan: Channel[T])(implicit context: ReversibleContext): T @cpsParam[Unit,Unit] = {
    shift { k: (T=>Unit) =>
      context.receiveClause() = {  
       case Message(msg: T, chanId, mTime) if chanId == chan.id =>
          chan.sender.future.map { ref => ref ! Ack(chan.id) }
          // pick the time that is higher
          if(mTime > context.time) context.time = mTime
          k.apply(msg)
        case m @ Message(msg, chanId, _) =>
          // hack: send back to self 
          context.selfRef ! m
      }
    }   
  }

  implicit def chanView2chan[T](chanView: ChannelView[T]): Channel[T] = {
    chanView.chan
  }

}

trait ReversibleContext {
  var sendClause: Ref.View[PartialFunction[Any,Unit]]
  var receiveClause: Ref.View[PartialFunction[Any,Unit]]
  val selfRef: ActorRef
  var time = 0

  // For our "easymode" implementation we can only have one channel.
  var chan: ChannelView[Int]

}

trait TimeDirection
object Forwards extends TimeDirection
object Backwards extends TimeDirection

// A process gets a view over a channel that includes some
// information. For sending use implicit conversion to inner channel.
case class ChannelView[T](chan: Channel[T]) {
  
  // Time direction
  var timeDirection: TimeDirection = Forwards
  // current time for this channel
  var time = 0
  val id: Int = chan.id

}

// A channel has a type, a sending end, and a receiving end. To avoid
// creation problems the channel is initialized with neither end
// present.

class Channel[T] {

  // a channel needs an ID so messages can be dispatched appropriately
  val id: Int = Globals.nextId

  // The uninitialized sender and receiver.
  var sender = Promise[ActorRef]()
  var receiver = Promise[ActorRef]()

  // This currently just sets the sender and receiver fields. What about 

  def register(role: ChannelRole)(implicit context: ReversibleContext) {
    context.chan = ChannelView[Int](this.asInstanceOf[Channel[Int]])
    role match {
      case ChannelSender => sender.trySuccess(context.selfRef)
      case ChannelReceiver => receiver.trySuccess(context.selfRef)
    }
  }

}

trait ChannelRole
case object ChannelSender extends ChannelRole
case object ChannelReceiver extends ChannelRole

// Box the messages sent on these channels so that they can be
// properly associated with the channels.

case class Message[T](msg: T, chanId: Int, time: Int)
case class Ack(chanId: Int)

// Some global information and utility functions.

object Globals {

  var id = 0
  def nextId: Int = {
    id = id + 1
    id
  }

}

// Our testing system

object TestSystem {

  val system = ActorSystem()

  import Functions._
/*
  class Foo[T]
  val foo = new Foo[Unit@cpsParam[Unit,Unit]]
*/
  def test {
    println("Running test.")

    val channel = new Channel[Int]()
    /*
    val senderTask = { implicit context: ReversibleContext =>
      reset {
        channel.register(ChannelSender)
        println("sender registered, about to send")
        send(channel, 120)

        println("Simple backtrack test:")
        val work = stable[Int,Unit@cpsParam[Unit,Unit]] { (input: Int, backtrack: Backtrack[Int]) =>
          println("Input: " + input)
          if(input == 0) {
            println("Found zero")
            send(channel, input)
            println("post send")
          } else {
            backtrack(input-1)
            send(channel, input)
            println("post send")
          }
          println("lol")
        }

        work(5)
        send(channel, 137)
      }
    }

    val receiverTask = { implicit context: ReversibleContext =>
      reset {
        channel.register(ChannelReceiver)
        println("receiver registered, about to receive")
        println("Our received message is: " + receive(channel))
        
        val work = stable[Int,Unit] { (input: Int, backtrack: Backtrack[Int]) =>
          println("printing stuff: " + receive(channel) + " " + input)
        }
        work(50120)
        println("Finished: " + receive(channel))
      }
    }

    val receiverActor = system.actorOf(Props(new ProcessActor(receiverTask)))
    val senderActor = system.actorOf(Props(new ProcessActor(senderTask)))

    Thread.sleep(1000)
    system.shutdown()
*/
    println("Ran test.")
  }

}

