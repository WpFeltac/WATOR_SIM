package app

final case class GridStep(action: GridAction, oldCoord: Coord, newCoord: Coord)
