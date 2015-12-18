package game

case class ShapeCoord(x: Int, y: Int, fromX: Option[Int], fromY: Option[Int], toX: Option[Int], toY: Option[Int])

object Screen {
  def pixelsPerShape(tilePixels: Int) = tilePixels * Const.tilesPerShape

  def pixelsToTilesOffset(screenOffs: (Int, Int), tilePixels: Int): (Int, Int) =
    (screenOffs._1 / tilePixels, screenOffs._2 / tilePixels)

  def shapesCoordsWithCutOffsets(tileOffset: (Int, Int)): Seq[ShapeCoord] =
    List(
      ShapeCoord(0, 0, Some(tileOffset._1), Some(tileOffset._2), None, None),
      ShapeCoord(0, 1, Some(tileOffset._1), None, None, Some(tileOffset._2)),
      ShapeCoord(1, 0, None, Some(tileOffset._2), Some(tileOffset._1), None),
      ShapeCoord(1, 1, None, None, Some(tileOffset._1), Some(tileOffset._2))
    )

  def cutFromCoords(terrain: Map[(Int, Int), Shape], coords: Seq[ShapeCoord]): Map[(Int, Int), Shape] =
    coords.map {
      case ShapeCoord(x, y, fromX, fromY, toX, toY) =>
        val shape = terrain.get((x, y)).get.tiles
        ((x, y), Shape(shape.slice(fromX.getOrElse(0), toX.getOrElse(shape.length)).map(row =>
          row.slice(fromY.getOrElse(0), toY.getOrElse(row.length)))
        ))
    }.toMap

  def mapToTab(cutShapes: Map[(Int, Int), Shape]): Seq[Seq[Array[Array[Boolean]]]] =
    List(
      List(cutShapes.get((0, 0)).get.tiles, cutShapes.get((0, 1)).get.tiles),
      List(cutShapes.get((1, 0)).get.tiles, cutShapes.get((1, 1)).get.tiles)
    )

  def joinRow(row: Seq[Array[Boolean]]): Array[Boolean] = row.reduce(_ ++ _)

  def rehash[A](oneWay: Seq[Array[A]]): Array[Seq[A]] = {
    def rehashAcc[A](acc: Array[Seq[A]], oneWay: Seq[Array[A]]): Array[Seq[A]] = {
      if(oneWay.head.isEmpty) acc
      else rehashAcc(acc :+ oneWay.map(_.head), oneWay.map(_.tail))
    }
    rehashAcc(Array[Seq[A]](), oneWay)
  }

  def joinShapes(tabOfShapes: Seq[Seq[Array[Array[Boolean]]]]): Shape = {
    Shape(
      tabOfShapes.head.head.zip(tabOfShapes.head(1)).map(r => r._1 ++ r._2) ++
        tabOfShapes(1).head.zip(tabOfShapes(1)(1)).map(r => r._1 ++ r._2)
    )
  }

  def cutDisplayed(terrain: Map[(Int, Int), Shape], tileOffset: (Int, Int)): Shape =
    joinShapes(mapToTab(cutFromCoords(terrain, shapesCoordsWithCutOffsets(tileOffset))))

  def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  def fluentDiv(a: Int, b: Int) = if(a < 0) a / b - 1 else a / b

  def calculate(player: Player, tilePixels: Int): Shape = {
    val pix = pixelsPerShape(tilePixels)
    val shapeCoord = (fluentDiv(player.x, pix), fluentDiv(player.y, pix))
    val screenOffs = (absModulo(player.x, pix), absModulo(player.y, pix))
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeCoord match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain: Map[(Int, Int), Shape] = terrainCoords.map { case (dx, dy) =>
      ((dx - shapeCoord._1, dy - shapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
    cutDisplayed(terrain, pixelsToTilesOffset(screenOffs, tilePixels))
  }
}



