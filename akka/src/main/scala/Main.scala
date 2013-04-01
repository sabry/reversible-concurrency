
// A pointlessly complicated hello world.

package renamethispackage

import akka.actor.{ ActorSystem, Actor, Props }
import scala.util.continuations._

case object DoSomething

object Main {
  def main(args: Array[String]): Unit = {

    // Actual reversible concurrency trigger
    import reversibleconcurrency3._
    TestSystem.test

    // continuations stuff
    import ContinuationTest._
    really
    suspendThenResume()
    beInteresting()

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

  def really {
    reset {
      suspend
      println(120)
    }
    resume
  }

  def go() {

    val x = reset {
      shift { k: (Int => Int) =>
        k(24)
      } * 5
    }

    println(x)
  }  

  def suspendThenResume() {

    var saved: Option[Unit=>Unit] = None
    reset {
      shift { k: (Unit=>Unit) =>
        saved = Some(k)
      }
      println("Found 120 yet another way.")
    }

    saved.get()

  }

  var cont: Option[Unit=>Unit] = None

  def suspend: Unit @ cpsParam[Unit,Unit] = {
    shift { k: (Unit=>Unit) =>
      cont = Some(k)
    }
  }

  def resume {
    cont.get.apply()
  }

  def beInteresting() = {
    reset {
      println("1")
      suspend
      println("2")
    }
    println("pre-resume")
    resume
    resume
  }

}



