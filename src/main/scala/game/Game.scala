package game

import akka.actor._

case class Step(step: Int)

object StepActor {
  def props() = Props(classOf[StepActor])
}
class StepActor extends Actor with ActorLogging {
  val words = Array("g", "w", "t", "e", "h")
  var prefix = ""

  def receive = {
    case Step(step) => {
      context.actorSelection("../websocket") ! Push(prefix + words(step))
    }

    case UserInput("right") => {
      prefix += "X"
    }

    case UserInput("left") => {
      prefix = if(prefix.length > 0) prefix.substring(1) else prefix
    }
  }
}

//TODO possible/planned actors:
//Clock (currently Game object)
//Step
//State (currenty in Step)

object Game {
  def play(stepActor: ActorRef) = {
    for(index <- Stream.iterate(0)(x => if(x == 4) 0 else x + 1)) {
      stepActor ! Step(index)
      Thread.sleep(50)
    }
  }
}