package game.multiplayer

import java.util.concurrent.atomic.AtomicLong
import java.util.function.LongUnaryOperator

import akka.http.scaladsl.model.DateTime
import game.Const

case class SinglePlayerPublicState(position: (Int, Int), latestHeartbeat: DateTime)

object GlobalState {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)

  private val playerCounter = new AtomicLong(0L)

  def getPlayerId(): Long = {
    def longUnaryOperator = new LongUnaryOperator {
      override def applyAsLong(operand: Long): Long = operand + 1L
    }
    playerCounter.updateAndGet(longUnaryOperator)
  }

  private val playersPublicData = new scala.collection.concurrent.TrieMap[Long, SinglePlayerPublicState]

  def publishState(playerId: Long, position: (Int, Int)): Unit = {
    val publicState = SinglePlayerPublicState(position, DateTime.now)
    playersPublicData.put(playerId, publicState)
  }

  def getPositionsWithinRectangle(upperLeft: (Int, Int), lowerRight: (Int, Int)): Seq[(Long, SinglePlayerPublicState)] =
    playersPublicData.toSeq.filter { case (id, SinglePlayerPublicState(pos, _)) =>
      pos._1 >= upperLeft._1 && pos._2 >= upperLeft._2 && pos._1 < lowerRight._1 && pos._2 < lowerRight._2
    }

  def deleteIdlePlayers(): Unit = {
    val currTime = DateTime.now
    val toDelete = playersPublicData.toSeq.filter {
      case (_, SinglePlayerPublicState(_, latestHeartbeat)) =>
        currTime - 10000 > latestHeartbeat
    }
    toDelete.foreach {
      case (id, _) =>
        playersPublicData.remove(id)
    }
  }
}