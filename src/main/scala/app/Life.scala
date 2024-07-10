package app

import app.FishType.{SHARK, TUNA}
import app.GridAction.{DIE, MOVE, NOTHING, REPRODUCE}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.annotation.tailrec
import scala.util.Random

final case class Life(grid : Map[(Int, Int), FishType], fishList: List[Fish], agentSize: Int, gridBound: Int) {
    def draw(): List[Rectangle] = {
        // Generating corresponding shapes to draw
        val shapeList = grid.map(e =>
            e._2 match
                case TUNA =>
                    new Rectangle {
                        x = e._1._1 * agentSize
                        y = e._1._2 * agentSize
                        width = agentSize
                        height = agentSize
                        fill = Color.rgb(189, 105, 76)
                    }
                case SHARK =>
                    new Rectangle {
                        x = e._1._1 * agentSize
                        y = e._1._2 * agentSize
                        width = agentSize
                        height = agentSize
                        fill = Color.rgb(42, 72, 99)
                    }
        ).toList

        shapeList
    }

    def move(tBreed: Int, sBreed: Int, sEnergy: Int): Life = {

        val moveResult = fishList.foldLeft((List[Fish](), grid)) { (lg, f) =>
              f match
                  case f: Tuna =>
                      getRandomFreeNearbyCellCoord(lg._2, f.position) match
                          case Some(newCoord) =>
                              // Déplacement du thon
                              if (f.breed + 1 >= tBreed) {
                                  // Reproduction à notre ancienne position
                                  val updatedList = lg._1 :+ Tuna(f.position, 0) :+ Tuna(newCoord, 0)
                                  val updatedGrid = lg._2.updated((newCoord.x, newCoord.y), TUNA)
                                  (updatedList, updatedGrid)
                              }
                              else {
                                  // Déplacement sans reproduction
                                  val updatedList = lg._1 :+ Tuna(newCoord, f.breed + 1)
                                  val updatedGrid = lg._2.removed((f.position.x, f.position.y)).updated((newCoord.x, newCoord.y), TUNA)
                                  (updatedList, updatedGrid)
                              }
                          case None =>
                              // Pas de déplacement du thon
                              lg
                  case f: Shark =>
                      getNextTunaOccupiedCellCoord(lg._2, f.position) match
                          case Some(c) =>
                              // Déplacement avec repas
                              val eatenTunaGrid = lg._2.removed((c.x, c.y))
                              moveShark(lg, f.copy(energy = f.energy + 1), Some(c), sBreed, sEnergy)
                          case None =>
                              getRandomFreeNearbyCellCoord(lg._2, f.position) match
                                  // Déplacement sans repas
                                  case Some(c) =>
                                      moveShark(lg, f, Some(c), sBreed, sEnergy)
                                  // Pas de déplacement
                                  case None =>
                                      moveShark(lg, f, None, sBreed, sEnergy)
          }

        copy(fishList = moveResult._1, grid = moveResult._2)
    }

    private def getRandomFreeNearbyCellCoord(fishGrid: Map[(Int, Int), FishType], pos: Coord): Option[Coord] = {
        val nearbyCells = for {
            i <- (-1 to 1).toList
            j <- (-1 to 1).toList if (i, j) != (0, 0) &&
              pos.x + i >= 0 && pos.y + j >= 0 &&
              pos.x + i < gridBound && pos.y + j < gridBound &&
              !fishGrid.exists(g => g._1 == (pos.x + i, pos.y + j))

        } yield Coord(pos.x + i, pos.y + j)

        Random.shuffle(nearbyCells).headOption
    }

    private def getNextTunaOccupiedCellCoord(fishGrid: Map[(Int, Int), FishType], pos: Coord): Option[Coord] = {
        val nearbyCells = for {
            i <- (-1 to 1).toList
            j <- (-1 to 1).toList if (i, j) != (0, 0) &&
              pos.x + i >= 0 && pos.y + j >= 0 &&
              pos.x + i < gridBound && pos.y + j < gridBound &&
              fishGrid.exists(g => g._1 == (pos.x + i, pos.y + j)) &&
              fishGrid.get((pos.x + i, pos.y + j)).contains(TUNA)

        } yield Coord(pos.x + i, pos.y + j)

        Random.shuffle(nearbyCells).headOption
    }

    private def moveShark(lg: (List[Fish], Map[(Int, Int), FishType]), shark: Shark, newCoord: Option[Coord], sBreed: Int, sEnergy: Int): (List[Fish], Map[(Int, Int), FishType]) = {
        if(shark.energy - 1 > 0) {
            // Déplacement
            newCoord match
                case Some(newCoord) =>
                    // Déplacement
                    if (shark.breed + 1 >= sBreed) {
                        // Déplacement avec reproduction
                        val updatedList = lg._1 :+ Shark(shark.position, 0, sEnergy) :+ Shark(newCoord, 0, shark.energy - 1)
                        val updatedGrid = lg._2.updated((newCoord.x, newCoord.y), SHARK)
                        (updatedList, updatedGrid)
                    }
                    else {
                        // Déplacement sans reproduction
                        val updatedList = lg._1 :+ Shark(newCoord, shark.breed + 1, shark.energy - 1)
                        val updatedGrid = lg._2.removed((shark.position.x, shark.position.y)).updated((newCoord.x, newCoord.y), SHARK)
                        (updatedList, updatedGrid)
                    }
                // Pas de déplacement
                case None =>
                    lg
        }
        else {
            // Décès
            println("Shark Death reported")
            val updatedGrid = lg._2.removed((shark.position.x, shark.position.y))
            (lg._1, updatedGrid)
        }
    }
}