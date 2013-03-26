
package reversibleconcurrency

import akka.actor.{ ActorRef, Actor, ActorSystem, Props }
import scala.collection.mutable.HashSet
import scala.util.continuations._

trait TimeDirection
case object TForward extends TimeDirection
case object TBackward extends TimeDirection

trait MessageDirection
case object SendMode extends MessageDirection
case object ReceiveMode extends MessageDirection

case class Channel(sender: ActorRef, receiver: ActorRef)

case class ChannelView(raw: Channel, role: MessageDirection) {
  var time = 0
  var timeDirection: TimeDirection = TForward
}

case class Message(raw: Any, time: Int)

class ChannelMap {
  var raw = new HashSet[ChannelView]
}

case class Frame[Input,Output](time: Int, channelMap: ChannelMap, context: Context[Input,Output], value: Input) 

case class Context[Input,Output](k: Input=>Output)

object PactorSystem {
  
  val system = ActorSystem()

  import ProcessFunctions._

  val reallyFun = Task { implicit state: ProcessState =>
    println("a")
    suspend
    println("b")
  }         

  def work = {
    system.actorOf(Props(new Pactor(5, reallyFun)))
    Thread.sleep(1000)
    system.shutdown()
  }

}

class Pactor(id: Int, task: Task) extends Actor {

  import ProcessFunctions._

  def receive = {
    case m => println("id: " + id + " " + m)
  }

  implicit val state = new ProcessState(id)

  reset {
    task.run
  }

  resume

}

// build a mutable box that holds all the state for a process

class ProcessState(id: Int) {

  // continuation representing the top of the stable block
  // -- depth 1 cap for now
  var cont: Option[Unit=>Unit] = None

}

// The functions used by one of our reversible functions. Dynamic
// scope is gained by having each take a ProcessState implicitly. 
object ProcessFunctions {

  def suspend()(implicit state: ProcessState): Unit @ cpsParam[Unit,Unit] = {
    shift { k: (Unit=>Unit) =>
      state.cont = Some(k)
    }
  }

  def resume()(implicit state: ProcessState) {
    state.cont.get.apply()
  }

}

// WHAT IS THIS EVEN SUPPOSED TO DO!? AND HOW?!
class Task(work: OurTypes.Attempt) {
  def run(implicit state: ProcessState): Unit @cpsParam[Unit,Unit] = 
    work.apply(state)
}

object Task {
  def apply(work: OurTypes.Attempt): Task =
    new Task(work)
}

object OurTypes {

  type Attempt = ProcessState => Unit @cpsParam[Unit,Unit]

  // Not really using these right now?

  // For passing functions in to the task as arguments to work the
  // type signatures are going to be awful. This object/module
  // contains type aliases to make this a reasonable process.

  // To start we don't allow return values. Those are complicated...
  type Task = Unit

  // Suspend just suspends, nothing exciting
  type Suspend = Unit=>Unit 

  // Resume just resumes, nothing exciting
  type Resume = Unit=>Unit

  // Send expects a message, returns nothing
  type Send = Message => Unit

  // Receive expects nothing, returns a message
  type Receive = Unit => Message

}
 






