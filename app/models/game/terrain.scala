package game

object Terrain {
  sealed trait TerrainType
  case object Undefined extends TerrainType
  case object OutOfBound extends TerrainType
  case class Terrain(name: String, mouvModif: Int, symbol: Char, high: Int) extends TerrainType

  def mouvModif(terrain: TerrainType): Option[Int] = {
    terrain match {
      case Undefined => None
      case OutOfBound => None
      case tile: Terrain => Some(tile.mouvModif)
    }
  }

  def symb(terrain: TerrainType): Char = {
    terrain match {
      case Undefined => 'O'
      case OutOfBound => 'X'
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
