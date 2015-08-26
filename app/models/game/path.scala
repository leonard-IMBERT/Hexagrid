package game

import scala.math

import game.MapManager.Tile
import game.MapManager.Valid
import game.MapManager.WorldMap

object Path {
  case class Entity(coor: Valid)

  def findPath(wMap: WorldMap, start: Valid, end: Valid): List[(Int, Tile)] = {
    def go(acc: List[(Int, Tile)], end: Valid): List[(Int, Tile)] = {
      if (acc.last._2._1 == end) acc
      else  {
        val tmp = MapManager.getNeighbourgs(wMap, acc.last._2._1).tileMap.toList
          .collect(tupleTile => Terrain.mouvModif(tupleTile._2.terrain) match {
            case Some(i) => (distanceBeetween(tupleTile._1, end) + i, tupleTile)
          })
        .collect({case x if(!acc.exists(_ == x)) => x})
        .minBy(_._1)
        go(acc :+ tmp, end)
      }
    }
    wMap.tileMap.get(start).map((char) => (Terrain.mouvModif(char.terrain), (start, char))) match {
      case Some((Some(i), tile)) => go(List((i, tile)), end)
      case _ => List()
    }
  }

  def distanceBeetween(a: Valid, b: Valid): Int = {
    math.max(math.abs(a.x-b.x), math.max(math.abs(a.y-b.y), math.abs(a.z-b.z)))
  }
}
