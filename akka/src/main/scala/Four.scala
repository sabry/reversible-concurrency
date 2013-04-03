
package reversibleconcurrency4

import akka.actor.{ ActorRef, Actor, ActorSystem, Props }
import scala.util.continuations._
import scala.concurrent.stm._
import scala.concurrent.Promise
import scala.util.DynamicVariable

// Four.scala : 

// Added stable and backtrack functions using the Scala stack to store
// the backtracking continuation. No interaction with message passing
// yet.

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

//case class WorkUnit[In,Out](fun: (In=>Any)=>In=>Out)  {
//  def apply(input: In, implicit backtrack: In=>Any): Out = {
//    fun(backtrack)(input)
//  }
//}

//case class BackTrack[In](fun: (In,(In=>Any))=>Out) {
//  def apply(input: In, implicit backtrack: In=>Any) {
    
case class StableTask[In,Out](fun: (In,ReversibleContext) => Out @cpsParam[Out,Unit]) {
  def apply(input: In)(implicit context: ReversibleContext): Out @cpsParam[Out,Unit] = {
    fun(input, context)
  }
}

case class Backtrack[In](fun: (In,Backtrack[In])=>Any) {
  def apply(input: In): Any = {
    fun(input,this)
  }
}


object Functions {

//  implicit def bt2bt[In,Out](fun: (In=>Any)=>In=>Out): WorkUnit[In,Out] = {
//    WorkUnit(fun)
//  }

  // This is a functor parameterized by the system, but I need to
  // figure out the appropriate syntax. (aka I don't want this
  // import!)
  import TestSystem.system._

  def stable[In, Out](work: (In,Backtrack[In]) => Out): StableTask[In,Out] = {

    new StableTask[In,Out](
      (input: In, context: ReversibleContext) => {
        shift { k: (Out=>Any) =>          
          def bt = new Backtrack[In]( (input: In, bt: Backtrack[In]) =>
            k(work(input,bt))
          )
          work(input,bt)
        }
      }: Out @cpsParam[Out,Unit]
    )
    
  }
        
  def send[T](chan: Channel[T], msg: T)(implicit context: ReversibleContext): Unit @cpsParam[Unit,Unit] = {
    chan.receiver.future.map { 
      ref => ref ! Message(msg, chan.id)
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
       case Message(msg: T, chanId) if chanId == chan.id =>
          chan.sender.future.map { ref => ref ! Ack(chan.id) }
          k.apply(msg)
        case m @ Message(msg, chanId) =>
          // hack: send back to self 
          context.selfRef ! m
      }
    }   
  }

}

trait ReversibleContext {
  // timestamp stuff
  var sendClause: Ref.View[PartialFunction[Any,Unit]]
  var receiveClause: Ref.View[PartialFunction[Any,Unit]]
  val selfRef: ActorRef
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

case class Message[T](msg: T, chanId: Int)
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

  def test {
    println("Running test.")

    val channel = new Channel[Int]()
    
    val senderTask = { implicit context: ReversibleContext =>
      reset {
        channel.register(ChannelSender)
        println("sender registered, about to send")
        send(channel, 120)

        println("Simple backtrack test:")
        val work = stable[Int,Unit] { (input: Int, backtrack: Backtrack[Int]) =>
          println("Input: " + input)
          if(input == 0) {
            println("Found zero")
          } else {
            backtrack(input-1)
          }
        }

        work(5)

/*
        println("lets try some backtracking, no message passing yet")
        var found = false
        def myWork(input: Int) { 
          val n = scala.util.Random.nextInt(input)
          println("n = " + n)
          if(n == 0) {
            println("found 0, stopping")   
            found = true
          } else {
            println("nozero, backtracking")            
            backtrack
          }
        }        
        while(!found) {
          stable(myWork _ , 5)
        }
        */
      }
    }

    val receiverTask = { implicit context: ReversibleContext =>
      reset {
        channel.register(ChannelReceiver)
        println("receiver registered, about to receive")
        println("Our received message is: " + receive(channel))
      }
    }

    val receiverActor = system.actorOf(Props(new ProcessActor(receiverTask)))
    val senderActor = system.actorOf(Props(new ProcessActor(senderTask)))

    Thread.sleep(1000)
    system.shutdown()

    println("Ran test.")
  }

}

