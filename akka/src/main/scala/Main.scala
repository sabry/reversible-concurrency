
// A pointlessly complicated hello world.

package renamethispackage

import akka.actor.{ ActorSystem, Actor, Props }

case object DoSomething

object Main {
  def main(args: Array[String]): Unit = {
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

