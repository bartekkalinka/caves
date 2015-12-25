package game

case class CutParams(leftTileOffset: (Int, Int), shapeSpan: (Int, Int), rightTileOffset: (Int, Int))

case class ShapeCoord(x: Int, y: Int, fromX: Option[Int], fromY: Option[Int], toX: Option[Int], toY: Option[Int])

object Screen {
  def pixelsPerShape(tilePixels: Int) = tilePixels * Const.tilesPerShape

  def pixelsToTilesOffset(screenOffs: (Int, Int), tilePixels: Int): (Int, Int) =
    (screenOffs._1 / tilePixels, screenOffs._2 / tilePixels)

  def shapesCoordsWithCutOffsets(cutParams: CutParams): Seq[ShapeCoord] =
    Seq.tabulate(cutParams.shapeSpan._1 + 1, cutParams.shapeSpan._2 + 1)((x, y) => (x, y)).flatten.map {
      case (x, y) =>
        ShapeCoord(x, y,
          if(x == 0) Some(cutParams.leftTileOffset._1) else None,
          if(y == 0) Some(cutParams.leftTileOffset._2) else None,
          if(x == cutParams.shapeSpan._1) Some(cutParams.rightTileOffset._1) else None,
          if(y == cutParams.shapeSpan._2) Some(cutParams.rightTileOffset._2) else None)
    }

  def cutFromCoords(terrain: Map[(Int, Int), Shape], coords: Seq[ShapeCoord]): Map[(Int, Int), Shape] =
    coords.map {
      case ShapeCoord(x, y, fromX, fromY, toX, toY) =>
        val shape = terrain.get((x, y)).get.tiles
        ((x, y), Shape(shape.slice(fromX.getOrElse(0), toX.getOrElse(shape.length)).map(row =>
          row.slice(fromY.getOrElse(0), toY.getOrElse(row.length)))
        ))
    }.toMap

  def mapKeysDims(map: Map[(Int, Int), Shape]): (Seq[Int], Seq[Int]) = (
    map.keys.map(_._1).toSeq.distinct.sortBy(identity),
    map.keys.map(_._2).toSeq.distinct.sortBy(identity)
  )

  def mapToTab(cutShapes: Map[(Int, Int), Shape]): Seq[Seq[Array[Array[Boolean]]]] = {
    val (seqX, seqY) = mapKeysDims(cutShapes)
    seqX.map(x =>
      seqY.map(y =>
        cutShapes.get((x, y)).get.tiles
      )
    )
  }

  def joinRow(row: Seq[Array[Boolean]]): Array[Boolean] = row.reduce(_ ++ _)

  def rehash[A](oneWay: Seq[Array[A]]): Array[Seq[A]] = {
    def rehashAcc(acc: Array[Seq[A]], oneWay: Seq[Array[A]]): Array[Seq[A]] = {
      if(oneWay.head.isEmpty) acc
      else rehashAcc(acc :+ oneWay.map(_.head), oneWay.map(_.tail))
    }
    rehashAcc(Array[Seq[A]](), oneWay)
  }

  def joinShapes(tabOfShapes: Seq[Seq[Array[Array[Boolean]]]]): Shape =
    Shape(
      tabOfShapes.map(rehash(_).map(joinRow)).reduce(_ ++ _)
    )

  def cutDisplayed(terrain: Map[(Int, Int), Shape], cutParams: CutParams): Shape = {
    val coords = shapesCoordsWithCutOffsets(cutParams)
    val shapesMap = cutFromCoords(terrain, coords)
    val shapesTab = mapToTab(shapesMap)
    joinShapes(shapesTab)
  }

  def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  def fluentDiv(a: Int, b: Int) = if(a < 0) a / b - 1 else a / b

  def getTerrainFromGenerator(shapeOffset: (Int, Int)): Map[(Int, Int), Shape] = {
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeOffset match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    terrainCoords.map { case (dx, dy) =>
      ((dx - shapeOffset._1, dy - shapeOffset._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
  }

  def calculate(player: Player, tilePixels: Int): Shape = {
    val pix = pixelsPerShape(tilePixels)
    val shapeOffset = (fluentDiv(player.x, pix), fluentDiv(player.y, pix))
    val leftPixelOffset = (absModulo(player.x, pix), absModulo(player.y, pix))
    val shapeSpan = ((leftPixelOffset._1 + Const.screenWidth) / pix, (leftPixelOffset._2 + Const. screenHeight) / pix)
    val rightPixelOffset = ((leftPixelOffset._1 + Const.screenWidth) % pix, (leftPixelOffset._2 + Const. screenHeight) % pix)
    val terrain: Map[(Int, Int), Shape] = getTerrainFromGenerator(shapeOffset)
    val cutParams = CutParams(pixelsToTilesOffset(leftPixelOffset, tilePixels),
      shapeSpan, pixelsToTilesOffset(rightPixelOffset, tilePixels))
    cutDisplayed(terrain, cutParams)
  }
}



