package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case object Tick
case class StateData(prefix: String, letter: String)
case class UserInput(dir: Option[String])
case class StepData(state: StateData, input: UserInput)
case class StateMod(addToPrefix: Int)

object Step {
  def props() = Props(classOf[Step])
}
class Step extends Actor with ActorLogging {
  //TODO layers of state modification, one triggering next
  def receive = {
    case StepData(state, input) => {
      log.debug("StepData received, input = " + input.dir.getOrElse("null"))
      val addToPrefix = input.dir match {
          case Some ("right") => 1
          case Some("left") => -1
          case None => 0
      }
      context.actorSelection("../state") ! StateMod(addToPrefix)
    }
  }
}

object Input {
  def props() = Props(classOf[Input])
}
class Input extends Actor with ActorLogging {
  var userInput: Option[String] = None

  def receive = {
    case UserInput(ui) => {
      //log.debug("UserInput " + ui.get + " received")
      userInput = ui
    }
    case sd: StateData => {
      log.debug("StateData received")
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

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case StateMod(addToPrefix) => {
      log.debug("StateMod received, addToPrefix = " + addToPrefix)
      //apply modifications
      //TODO state should be the last layer of state modification
      //TODO separate into a method
      //TODO move index modification to Step
      val prefixLength = Math.max(prefix.length + addToPrefix, 0)
      log.debug("prefixLength " + prefixLength)
      prefix = List.tabulate(prefixLength)(x => "X").mkString("")
      log.debug("prefix |" + prefix + "|")
      //change index
      index = if(index == 4) 0 else index + 1

      //broadcast state to client
      //TODO separate into a method
      context.actorSelection("../websocket") ! Push(prefix + words(index))
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(prefix, words(index))
    }
  }
}
