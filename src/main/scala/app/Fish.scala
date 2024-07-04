package app

sealed trait Fish(val position: Coord, val breed: Int)

final case class Tuna(override val position: Coord, override val breed: Int) extends Fish(position, breed)
final case class Shark(override val position: Coord, override val breed: Int, energy: Int) extends Fish(position, breed)