package websockets

import akka.actor.Cancellable
import akka.stream.{Attributes, FanInShape2}
import akka.stream.scaladsl._
import akka.stream.stage.{DownstreamDirective, UpstreamDirective, DetachedContext, DetachedStage}
import scala.concurrent.duration._
import game._

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

object Transformations {
  def zipWithNode[A](implicit builder: FlowGraph.Builder[Unit]): FanInShape2[Unit, A, A] =  {
    val zipWith = ZipWith[Unit, A, A]((a: Unit, i: A) => i)
    val zipWithSmallBuffer = zipWith.withAttributes(Attributes.inputBuffer(initial = 1, max = 1))
    builder.add(zipWithSmallBuffer)
  }

  def zipWithTick[A, B](tick: Source[Unit, B], toZip: Source[A, Unit]): Source[A, Unit] =
    Source() { implicit builder: FlowGraph.Builder[Unit] =>
      import FlowGraph.Implicits._
      val zipNode = zipWithNode[A]
      tick ~> zipNode.in0
      toZip ~> zipNode.in1
      zipNode.out
    }

  def lastElemOption[A](tick: Source[Unit, Cancellable], source: Source[A, Unit]): Source[Option[A], Unit] = {
    val pipedSource = source.transform(() => new LastElemOption[A]())
    zipWithTick(tick, pipedSource)
  }
}

object WsFlow {
  def mainTick: Source[Unit, Cancellable] = Source(1 seconds, 50 millis, ())

  def wsFlow(sender: String): Flow[UserInput, game.Broadcast, Unit] =
    Flow() { implicit builder: FlowGraph.Builder[Unit] =>
      import FlowGraph.Implicits._
      val front = Flow[game.UserInput].map(identity)
      val frontNode = builder.add(front)
      val sync = frontNode.outlet.transform(() => new LastElemOption[UserInput]())
      val zipNode = Transformations.zipWithNode[Option[UserInput]]
      mainTick ~> zipNode.in0
      sync.outlet ~> zipNode.in1
      val state = Flow[Option[UserInput]].scan(game.State.init) { (state: State, input: Option[UserInput]) =>
        state.applyMod(Step.step(StepData(StateData(state.player), input)))
      }
      val stateNode = builder.add(state)
      zipNode.out ~> stateNode.inlet
      val broadcast = stateNode.outlet.map(game.Broadcast.fromState)
      (frontNode.inlet, broadcast.outlet)
  }
}
