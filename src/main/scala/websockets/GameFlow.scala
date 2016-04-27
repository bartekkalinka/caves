package websockets

import akka.NotUsed
import akka.actor.Cancellable
import akka.stream.scaladsl._
import akka.stream.stage.{DetachedContext, DetachedStage, DownstreamDirective, UpstreamDirective}
import akka.stream.{FlowShape, Attributes, FanInShape2}
import game._
import game.singleplayer.{SinglePlayerState, UserInput}
import scala.concurrent.duration._

object GameFlow {
  private def zipWithNode[A](implicit builder: GraphDSL.Builder[NotUsed]): FanInShape2[Unit, A, A] =  {
    val zipWith = ZipWith[Unit, A, A]((a: Unit, i: A) => i)
    val zipWithSmallBuffer = zipWith.withAttributes(Attributes.inputBuffer(initial = 1, max = 1))
    builder.add(zipWithSmallBuffer)
  }

  def flow(sender: String): Flow[UserInput, singleplayer.Broadcast, NotUsed] =
    Flow.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._
      val mainTick = Source.tick(1 seconds, 50 millis, ())
      val syncNode = builder.add(
        Flow[UserInput]
          .conflate((acc, elem) => elem)
         .expand[Option[UserInput]](elem => Iterator(Some(elem)) ++ Iterator.continually(None))
      )
      val zipNode = zipWithNode[Option[UserInput]]
      val stateNode = builder.add(Flow[Option[UserInput]].scan(SinglePlayerState.init)(game.singleplayer.SinglePlayerState.iteration))
      val broadcastNode = builder.add(Flow[SinglePlayerState].map(game.singleplayer.Broadcast.fromState))

      mainTick ~> zipNode.in0
      syncNode.outlet ~> zipNode.in1
      zipNode.out ~> stateNode.in
      stateNode.outlet ~> broadcastNode.in
      FlowShape(syncNode.in, broadcastNode.outlet)
  })
}
