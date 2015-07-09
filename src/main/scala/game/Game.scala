package game

import akka.actor._
import scala.concurrent.duration._

case object Tick
case class StateData(prefix: String, letter: String)
case class UserInput(dir: Option[String])
case class StepData(state: StateData, input: UserInput)
case class StateMod(addToPrefix: Int)

object Step {
  def props() = Props(classOf[Step])
}
class Step extends Actor with ActorLogging {
  def receive = {
    case StepData(state, input) => {
      val addToPrefix = input.dir match {
          case Some ("right") => 1
          case Some("left") => -1
          case None => 0
      }
    }
  }
}

object Input {
  def props() = Props(classOf[State])
}
class Input extends Actor with ActorLogging {
  var userInput: Option[String] = None

  def receive = {
    case UserInput(ui) => userInput = ui
    case sd: StateData => {
      context.actorSelection("../step") ! StepData(sd, UserInput(userInput))
      userInput = None
    }
  }
}

object State {
  def props() = Props(classOf[State])
}
class State extends Actor with ActorLogging {
  val stream = Stream.iterate(0)(x => if(x == 4) 0 else x + 1)
  val words = Array("g", "w", "t", "e", "h")
  var prefix = ""
  var index = 0

  override def preStart(): Unit = { context.system.scheduler.scheduleOnce(1 seconds,self, Tick) }

  def receive = {
    case StateMod(addToPrefix) => {
      //apply modifications
      //TODO separate into a method
      val prefixLength = Math.max(prefix.length + addToPrefix, 0)
      prefix = List.tabulate(prefixLength)(x => " ").mkString("")
      //TODO change index

      //broadcast state to client
      //TODO separate into a method
      context.actorSelection("../websocket") ! Push(prefix + words(index))
    }

    case Tick => context.actorSelection("../input") ! StateData(prefix, words(index))
  }

}

//TODO possible/planned actors:
//Clock (currently Game object)
//Step
//State (currenty in Step)