package game

import scala.math

import play.api._

object Drawer {
  
  type Matrice = Map[(Double, Double), MapManager.Charac]

  def drawMatrice(wMap: MapManager.WorldMap): Matrice = {
    wMap.tileMap.toList.foldLeft(Map(): Matrice)((matrice, tile: MapManager.Tile) => {
      matrice + tileToMatrice(tile, 10, 400, 400)
    })
  }

  def tileToMatrice(tile: MapManager.Tile, zoom: Double, decX: Int, decY: Int): ((Double, Double), MapManager.Charac) = {
    val x = (zoom * ((math.sqrt(3) * tile._1.z) + (math.sqrt(3) * tile._1.y)/2)) + decX
    val y = (zoom * 3/2 * tile._1.y) + decY
    ((x,y), tile._2)
  }

  def matriceToDrawMatrice(matrice: Matrice): List[((Long, Long), String)] = {
    matrice.toList.map((tuple) => ((math.round(tuple._1._1), math.round(tuple._1._2)), Terrain.symb(tuple._2.terrain)))
  }

  

  def drawMap(matrice: Matrice): String = {
    val roundedMatrice = matriceToDrawMatrice(matrice)
    val minimalValue: Long = math.abs(roundedMatrice.map(tuple => math.min(tuple._1._1, tuple._1._2)).min)
    val roundedPositiveMatrice = roundedMatrice.map(tuple => ((tuple._1._1 + minimalValue, tuple._1._2 + minimalValue), tuple._2))
    
    val maximalValue = roundedPositiveMatrice.map(tuple => math.max(tuple._1._1, tuple._1._2)).max

    def draw(acc: String, ite: Long, stop: Long, matrice: List[((Long, Long), String)]): String = {
      if(ite > stop) acc
      else {
        val row = matrice.collect({case ((x,y), c) if(y == ite) => (x, c)}).toMap
        def drawRow(acc: String, ite2: Long, rowD: Map[Long, String]): String = {
          if(ite2 == stop) acc + row.get(ite2).getOrElse(' ') + "\n"
          else {
            drawRow(acc + row.get(ite2).getOrElse(' '), ite2 + 1, rowD)
          }
        }
        draw(drawRow(acc, 0, row), ite + 1, stop, matrice)
      }
    }

    draw("",0, maximalValue, roundedPositiveMatrice)
  }
}
