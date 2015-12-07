package websockets

import akka.actor.Cancellable
import akka.stream.scaladsl._
import akka.stream.stage.{DetachedContext, DetachedStage, DownstreamDirective, UpstreamDirective}
import akka.stream.{Attributes, FanInShape2}
import game._
import scala.concurrent.duration._

class LastElemOption[T]() extends DetachedStage[T, Option[T]] {
  private var currentValue: Option[T] = None

  override def onPush(elem: T, ctx: DetachedContext[Option[T]]): UpstreamDirective = {
    currentValue = Some(elem)
    ctx.pull()
  }

  override def onPull(ctx: DetachedContext[Option[T]]): DownstreamDirective = {
    val previousValue = currentValue
    currentValue = None
    ctx.push(previousValue)
  }
}

object GameFlow {
  def mainTick: Source[Unit, Cancellable] = Source(1 seconds, 50 millis, ())

  private def zipWithNode[A](implicit builder: FlowGraph.Builder[Unit]): FanInShape2[Unit, A, A] =  {
    val zipWith = ZipWith[Unit, A, A]((a: Unit, i: A) => i)
    val zipWithSmallBuffer = zipWith.withAttributes(Attributes.inputBuffer(initial = 1, max = 1))
    builder.add(zipWithSmallBuffer)
  }

  private def oneIteration(state: State, input: Option[UserInput]) =
    state.applyMod(Step.step(StepData(StateData(state.player), input)))

  def flow(sender: String): Flow[UserInput, game.Broadcast, Unit] =
    Flow() { implicit builder: FlowGraph.Builder[Unit] =>
      import FlowGraph.Implicits._
      val front = Flow[game.UserInput].map(identity)
      val frontNode = builder.add(front)
      val sync = frontNode.outlet.transform(() => new LastElemOption[UserInput]())
      val zipNode = zipWithNode[Option[UserInput]]
      val state = Flow[Option[UserInput]].scan(game.State.init)(oneIteration)
      val stateNode = builder.add(state)
      val broadcast = stateNode.outlet.map(game.Broadcast.fromState)

      mainTick ~> zipNode.in0
      sync.outlet ~> zipNode.in1
      zipNode.out ~> stateNode.inlet
      (frontNode.inlet, broadcast.outlet)
  }
}
