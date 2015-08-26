package game

import scala.math

object Drawer {
  
  type Matrice = Map[(Double, Double), MapManager.Charac]

  def drawMatrice(wMap: MapManager.WorldMap): Matrice = {
    wMap.tileMap.toList.foldLeft(Map(): Matrice)((matrice, tile: MapManager.Tile) => {
      matrice + tileToMatrice(tile, 1)
    })
  }

  def tileToMatrice(tile: MapManager.Tile, zoom: Double): ((Double, Double), MapManager.Charac) = {
    val x = zoom * math.sqrt(3) * (tile._1.x + tile._1.y/2)
    val y = zoom * 3/2 * tile._1.y
    ((x,y), tile._2)
  }

  def matriceToDrawMatrice(matrice: Matrice): List[((Long, Long), Char)] = {
    matrice.toList.map((tuple) => ((math.round(tuple._1._1), math.round(tuple._1._2)), Terrain.symb(tuple._2.terrain)))
  }

  def drawMap(matrice: Matrice): Unit = {
    val roundedMatrice = matriceToDrawMatrice(matrice)
    val minimalValue: Long = math.abs(roundedMatrice.map(tuple => math.min(tuple._1._1, tuple._1._2)).min)
    val roundedPositiveMatrice = roundedMatrice.map(tuple => ((tuple._1._1 + minimalValue, tuple._1._2 + minimalValue), tuple._2))
    
    val maximalValue = roundedPositiveMatrice.map(tuple => math.max(tuple._1._1, tuple._1._2)).max

    def draw(ite: Long, stop: Long, matrice: List[((Long, Long), Char)]): Unit = {
      if(ite > stop) ()
      else {
        val row = matrice.collect({case ((x,y), c) if(y == ite) => (x, c)}).toMap
        def drawRow(ite2: Long, rowD: Map[Long, Char]): Unit = {
          if(ite2 == stop) println(row.get(ite2).getOrElse(' '))
          else {
            print(row.get(ite2).getOrElse(' '))
            drawRow(ite2 + 1, rowD)
          }
        }
        drawRow(0, row)
        draw(ite + 1, stop, matrice)
      }
    }

    draw(0, maximalValue, roundedPositiveMatrice)
  }
}
