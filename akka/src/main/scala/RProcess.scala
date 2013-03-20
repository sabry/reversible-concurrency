
package reversibleconcurrency

import akka.actor.{ ActorRef }
import scala.collection.mutable.HashSet

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

//case class Frame[Input,Output](time: Int, channelMap: ChannelMap, context: FooContext[Input,Output], value: Input) 

//case class FooContext[Input,Output]
