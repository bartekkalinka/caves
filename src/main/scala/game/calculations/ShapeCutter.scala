package game.calculations

import game.state.Shape

case class TerrainSliceWithCutParams(terrainSlice: Map[(Int, Int), Shape], upperLeftPixelOffset: (Int, Int), shapeSpan: (Int, Int), lowerRightPixelOffset: (Int, Int))

case class ShapeCoord(x: Int, y: Int, fromX: Option[Int], fromY: Option[Int], toX: Option[Int], toY: Option[Int])

case class ShapeCutter(tilePixels: Int) {
  def cut(cutParams: TerrainSliceWithCutParams): Shape = {
    val coords = shapesCoordsWithCutOffsets(cutParams)
    val shapesMap = cutFromCoords(cutParams.terrainSlice, coords)
    val shapesTab = mapToTab(shapesMap)
    joinShapes(shapesTab)
  }

  private def shapesCoordsWithCutOffsets(cutParams: TerrainSliceWithCutParams): Seq[ShapeCoord] =
    Seq.tabulate(cutParams.shapeSpan._1, cutParams.shapeSpan._2)((x, y) => (x, y)).flatten.map {
      case (x, y) =>
        ShapeCoord(x, y,
          if(x == 0) Some(ScreenCommon.pixelsToTilesOffset(cutParams.upperLeftPixelOffset, tilePixels)._1) else None,
          if(y == 0) Some(ScreenCommon.pixelsToTilesOffset(cutParams.upperLeftPixelOffset, tilePixels)._2) else None,
          if(x == cutParams.shapeSpan._1 - 1) Some(ScreenCommon.pixelsToTilesOffset(cutParams.lowerRightPixelOffset, tilePixels)._1) else None,
          if(y == cutParams.shapeSpan._2 - 1) Some(ScreenCommon.pixelsToTilesOffset(cutParams.lowerRightPixelOffset, tilePixels)._2) else None)
    }

  private def cutFromCoords(terrain: Map[(Int, Int), Shape], coords: Seq[ShapeCoord]): Map[(Int, Int), Shape] =
    coords.map {
      case ShapeCoord(x, y, fromX, fromY, toX, toY) =>
        val shape = terrain.get((x, y)).get.tiles
        ((x, y), Shape(shape.slice(fromX.getOrElse(0), toX.getOrElse(shape.length)).map(row =>
          row.slice(fromY.getOrElse(0), toY.getOrElse(row.length)))
        ))
    }.toMap

  private def mapKeysDims(map: Map[(Int, Int), Shape]): (Seq[Int], Seq[Int]) = (
    map.keys.map(_._1).toSeq.distinct.sortBy(identity),
    map.keys.map(_._2).toSeq.distinct.sortBy(identity)
    )

  private def mapToTab(cutShapes: Map[(Int, Int), Shape]): Seq[Seq[Array[Array[Boolean]]]] = {
    val (seqX, seqY) = mapKeysDims(cutShapes)
    seqX.map(x =>
      seqY.map(y =>
        cutShapes.get((x, y)).get.tiles
      )
    )
  }

  private def joinRow(row: Seq[Array[Boolean]]): Array[Boolean] = row.reduce(_ ++ _)

  def rehash[A](oneWay: Seq[Array[A]]): Array[Seq[A]] = {
    def rehashAcc(acc: Array[Seq[A]], oneWay: Seq[Array[A]]): Array[Seq[A]] = {
      if(oneWay.head.isEmpty) acc
      else rehashAcc(acc :+ oneWay.map(_.head), oneWay.map(_.tail))
    }
    rehashAcc(Array[Seq[A]](), oneWay)
  }

  private def joinShapes(tabOfShapes: Seq[Seq[Array[Array[Boolean]]]]): Shape =
    Shape(
      tabOfShapes.map(rehash(_).map(joinRow)).reduce(_ ++ _)
    )
}


