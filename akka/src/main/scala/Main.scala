
// A pointlessly complicated hello world.

package renamethispackage

import akka.actor.{ ActorSystem, Actor, Props }
import scala.util.continuations._

case object DoSomething

object Main {
  def main(args: Array[String]): Unit = {

    // continuations stuff
    ContinuationTest.go()

    // actor stuff
    val system = ActorSystem()
    system.actorOf(Props[PointlessActor]) ! DoSomething
  }
}

class PointlessActor extends Actor {

  def receive = {
    case DoSomething =>
      println("The answer you seek is 120.")
      context.system.shutdown()
    case _ => 
      println("How did you actually manage to write this program incorrectly?!")
      context.system.shutdown()
  }

}

// test delimited continuations plugin
object ContinuationTest {

  def go() {

    val x = reset {
      shift { k: (Int => Int) =>
        k(24)
      } * 5
    }

    println(x)
  }  

}
