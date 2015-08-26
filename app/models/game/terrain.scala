package game

object Terrain {
  sealed trait TerrainType
  case object Undefined extends TerrainType
  case object OutOfBound extends TerrainType
  case class Terrain(name: String, mouvModif: Int, symbol: String, high: Int) extends TerrainType

  def mouvModif(terrain: TerrainType): Option[Int] = {
    terrain match {
      case Undefined => None
      case OutOfBound => None
      case tile: Terrain => Some(tile.mouvModif)
    }
  }

  def symb(terrain: TerrainType): String = {
    terrain match {
      case Undefined => "#000"
      case OutOfBound => "#444"
      case tile: Terrain => tile.symbol
    }
  }

  def getHigh(terrain: TerrainType): Option[Int] = {
    terrain match {
      case Undefined => None
      case OutOfBound => None
      case tile: Terrain => Some(tile.high)
    }
  }
}
