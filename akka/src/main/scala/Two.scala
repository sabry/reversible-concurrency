
package reversibleconcurrency

import akka.actor.{ ActorRef, Actor, ActorSystem, Props }
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.util.continuations._

// This time I'll implement the other direction, build synchronous
// messaging and channels *then* worry about the semantics.

class ProcessActor(task: ReversibleContext => Unit) extends Actor {
  
  // config
  import context._
  val tracing = false // set to true to print all received messages

  // avoid hmap problems by representing the state as a stored partial
  // function of how to handle a message

  implicit object Context extends ReversibleContext {

    var sendClause: PartialFunction[Any,Unit] = 
      { case m => println(m) }
    
    var receiveClause: PartialFunction[Any,Unit] =
      { case m => self ! m }

    val queue = self

  }

  val errorClause: PartialFunction[Any,Unit] =
    { case m => println("Received an erronous message: " + m) }

  val tracer: PartialFunction[Any,Any] =
    { case m => if(tracing) println("received: " + m); m }

  def receive = tracer andThen {
    case m: Ack => Context.sendClause(m)
    case m: Message[Any] => Context.receiveClause(m)
    case m => errorClause(m)
  }

  task.apply(Context)
  
}

object Functions {

  def send[T](chan: Channel[T], msg: T)(implicit context: ReversibleContext): Unit @cpsParam[Unit,Unit] = {
    chan.receiver.get ! Message(msg, chan.id)
    shift { k: (Unit=>Unit) =>
      println("Sending message... " + msg)
      context.sendClause = {
        case m @ Ack(chanId) if chanId == chan.id =>
          k.apply() 
        case m @ Ack(chanId) =>
          println("Error: a sending process received an erronous Ack: " + m)
      }
    }  
  }  
 
  def receive[T](chan: Channel[T])(implicit context: ReversibleContext): T @cpsParam[Unit,Unit] = {
    shift { k: (T=>Unit) =>
      context.receiveClause = { 
        case Message(msg: T, chanId) if chanId == chan.id =>
          chan.sender.get ! Ack(chan.id)
          k.apply(msg)
        case m @ Message(msg, chanId) =>
          // hack: send back to self 
          context.queue ! m
      }
    }   
  }

}

trait ReversibleContext {
  var sendClause: PartialFunction[Any,Unit]
  var receiveClause: PartialFunction[Any,Unit]
  val queue: ActorRef
}

// A channel has a type, a sending end, and a receiving end. To avoid
// creation problems the channel is initialized with neither end
// present.

class Channel[T] {

  // a channel needs an ID so messages can be dispatched appropriately
  val id: Int = Globals.nextId

  // The uninitialized sender and receiver.
  // todo : use promise/future here to remove mutability?
  var sender: Option[ActorRef] = None
  var receiver: Option[ActorRef] = None

  def register(role: ChannelRole)(implicit context: ReversibleContext) {
    role match {
      case ChannelSender => sender = Some(context.queue)
      case ChannelReceiver => receiver = Some(context.queue)
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

