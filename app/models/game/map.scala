package game

import play.api._

import scala.math._
import scala.math.abs

object MapManager {
  case class WorldMap(tileMap: Map[Valid, Charac])
  case class Charac(terrain: Terrain.TerrainType)

  type Tile = (Valid, Charac)

  val Water = Terrain.Terrain("Water", 5, "#007FFF", 1)
  val Grass = Terrain.Terrain("Grass", 1, "#00FF7F", 3)
  val Mountain = Terrain.Terrain("Mountain", 3 , "#603F00", 4)
  val Sand = Terrain.Terrain("Sand", 1, "#FFFF00", 2)

  val terrainListG = List(Water, Grass, Mountain, Sand)

  sealed trait Coordonee
  case class Valid(x: Int, y: Int, z: Int) extends Coordonee
  case object Invalid extends Coordonee

  def generateWholeMap(size: Int, center: Valid = Valid(0,0,0)): WorldMap = {
    val worldMap = genUndefinedMap(size, center)
    def go(ite: Int, wMap: WorldMap): WorldMap = {
      if(ite < 0) wMap
      else {
        go(ite - 1, getLayerAround(ite, wMap, center).foldLeft(wMap)((wMapAcc, coor) => setTerrainForTile(terrainListG, wMapAcc, coor)))
      }
    }

    go(size, worldMap)
  }

  def setTerrainForTile(terrainList: List[Terrain.Terrain], wMap: WorldMap, tile: Valid): WorldMap = {
    val neigh = getNeighbourgs(wMap, tile)
    val onlyNeig = removeTile(neigh, tile)
    val listNeig = onlyNeig.tileMap.toList

    if (listNeig.map(_._2.terrain).contains(Terrain.OutOfBound)) {
      Logger.debug("Water on " + tile.toString)
      addTile(wMap, tile, Charac(terrainList.minBy(_.high)), true)
    }
    else {
      val tmp = addTile(wMap, tile, Charac(determineTerrain(listNeig.map(_._2.terrain), terrainList)), true)
      tmp
    }
  }

  def determineTerrain(neigh: List[Terrain.TerrainType], terrainList: List[Terrain.Terrain]): Terrain.Terrain = {
    val redeableTerrain = neigh.map(Terrain.getHigh(_)).collect({case Some(high) => high})
    val moyHigh = math.round(redeableTerrain.sum.toDouble / redeableTerrain.length.toDouble)

    val flatTile: Terrain.Terrain = terrainList.collect({case ter: Terrain.Terrain if (ter.high == moyHigh) => ter}).headOption.getOrElse(Terrain.Terrain("Nothing", 0 , "#FFF", 0))
  
    val upperTile: Terrain.Terrain = terrainList.collect({case ter: Terrain.Terrain if(ter.high  == moyHigh + 1) => ter}).headOption.getOrElse(flatTile)
    val downerTile: Terrain.Terrain = terrainList.collect({case ter: Terrain.Terrain if(ter.high == moyHigh - 1) => ter}).headOption.getOrElse(flatTile)

    val salt = math.random

    if(salt < 0.2) downerTile
    else if(salt >= 0.2 && salt <= 0.7) flatTile
    else upperTile
  }

  def getLayerAround(layer: Int, wMap: WorldMap, center: Valid): List[Valid] = {
    val li = for {
      x <- around(layer)(center.x)
      y <- around(layer)(center.y)
    } yield {
      newCoordonee(x,y,0-x-y)
    }

    li.collect({case co:Valid => co}).filter(co => (abs(co.x) <= center.x + layer && abs(co.y) <= center.y + layer && abs(co.z) <= center.z + layer) && (abs(co.x) == center.x + layer || abs(co.y) == center.y + layer || abs(co.z) == center.z + layer))
  }

  def genUndefinedMap(size: Int, center: Valid): WorldMap = {
    getPotentialNeighbourgs(center, size).foldLeft(WorldMap(Map()))((wMap, coor) => addTile(wMap, coor, Charac(Terrain.Undefined), true))
  }

  def genGrassMap: WorldMap = {
    getPotentialNeighbourgs(Valid(0,0,0), 4).foldLeft(WorldMap(Map()))((wMap, coor) => addTile(wMap, coor, Charac(Grass), true))
  }

  def newCoordonee(x: Int, y: Int, z: Int): Coordonee = {
    if(x + y + z == 0) Valid(x,y,z)
    else Invalid
  }

  def addTile(wMap: WorldMap, coor: Valid, char: Charac, over: Boolean): WorldMap = {
    wMap.tileMap.get(coor) match {
      case Some(tile2) => {
        if(over) WorldMap(wMap.tileMap + ((coor, char)))
        else if(tile2.terrain == Terrain.Undefined) WorldMap(wMap.tileMap + ((coor, char)))
        else wMap
      }
      case None => WorldMap(wMap.tileMap + ((coor, char)))
    }
  }

  def getPotentialNeighbourgs(coor: Valid, range: Int): List[Valid] = {
    val li = for{
      x <- around(range)(coor.x)
      y <- around(range)(coor.y)
      z <- around(range)(coor.z)
    } yield newCoordonee(x,y,z)
    filterCoor(li)
  }

  def getNeighbourgs(wMap: WorldMap, coor: Valid): WorldMap = {
    getPotentialNeighbourgs(coor, 1).foldLeft(WorldMap(Map()))((map, coor) => {
      wMap.tileMap.get(coor) match {
        case Some(char) => addTile(map, coor, char, true)
        case None => addTile(map, coor, Charac(Terrain.OutOfBound), false)
      }
    })
  }

  def removeTile(wMap: WorldMap, coor: Valid): WorldMap = WorldMap(wMap.tileMap - coor)

  def around(range: Int)(nu: Int): List[Int] = {
    val trueRange = abs(range)  
    val min = nu - trueRange
    val max = nu + trueRange
    
    def go(begin: Int, end: Int): List[Int] = {
      if (begin == end) List(begin)
      else begin :: go(begin + 1, end)
    }
    go(min, max)
  }

  def filterCoor(li: List[Coordonee]): List[Valid] = {
    li.collect({case x: Valid => x})
  }
}
