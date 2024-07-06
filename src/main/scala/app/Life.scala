package app

import app.FishType.{SHARK, TUNA}
import app.GridAction.{DIE, MOVE, NOTHING, REPRODUCE}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.annotation.tailrec
import scala.util.Random

final case class Life(grid : Map[(Int, Int), FishType], fishList: List[Fish], agentSize: Int, gridBound: Int) {
    def draw(): List[Rectangle] = {
        // On génère les rectangles à dessiner aux coordonnées correspondantes
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
        
        // On renvoie la liste des rectangles à dessiner
        shapeList
    }

    def move(tBreed: Int, sBreed: Int, sEnergy: Int): Life = {
        val moveFishResult: List[(Fish, GridStep)] = fishList.map {
            case fish: Tuna =>
                getRandomFreeNearbyCellCoord(grid, fish.position) match
                    case Some(newCoord) =>
                        // Déplacement du thon
                        if (fish.breed + 1 >= tBreed) {
                            // Reproduction à notre ancienne position
                            (fish.asInstanceOf[Tuna].copy(position = newCoord, breed = 0), GridStep(REPRODUCE, fish.position, newCoord, TUNA))
                        }
                        else {
                            // Déplacement sans reproduction
                            (fish.asInstanceOf[Tuna].copy(position = newCoord, breed = fish.breed + 1), GridStep(MOVE, fish.position, newCoord, TUNA))
                        }
                    case None =>
                        // Pas de déplacement du thon
                        (fish, GridStep(NOTHING, fish.position, fish.position, TUNA))
            case fish: Shark =>
                getNextTunaOccupiedCellCoord(grid, fish.position) match
                    case Some(c) =>
                        // Déplacement avec repas
                        val eatenTunaGrid = grid.removed((c.x, c.y))
                        moveShark(fish.asInstanceOf[Shark], Some(c), sBreed, sEnergy + 1, eatenTunaGrid)
                    case None =>
                        getRandomFreeNearbyCellCoord(grid, fish.position) match
                            // Déplacement sans repas
                            case Some(c) =>
                                moveShark(fish.asInstanceOf[Shark], Some(c), sBreed, sEnergy, grid)
                            // Pas de déplacement
                            case None => moveShark(fish.asInstanceOf[Shark], None, sBreed, sEnergy, grid)
        }

        /** Décrit la grille finale suite à tous les mouvements de poissons */
        val postFishMoveGrid: Map[(Int, Int), FishType] = getFishGrid(grid, moveFishResult.map(e => e._2), 0)
        val postFishMoveList: List[Fish] = moveFishResult.map(e => e._1)

        // Gestion les poissons qui :
        // - sont dans la grille mais pas dans la liste (issus de reproduction = ajout)
        // - sont dans la liste mais plus dans la grille (issus de mouvement ou de mort = suppression)

        // AJOUT
        // Récupération des positions nouvelles par rapport à la grille de base
        val newFishPos = postFishMoveGrid.toList.diff(moveFishResult.map(e => ((e._1.position.x, e._1.position.y), if(e.isInstanceOf[Tuna]) TUNA else SHARK)))

        // Création des poissons par rapport à ces positions
        val newFishList = newFishPos.map(pos =>
            pos._2 match
                case TUNA =>
                    Tuna(Coord(pos._1._1, pos._1._2), 0)
                case SHARK =>
                    Shark(Coord(pos._1._1, pos._1._2), 0, sEnergy)

        )

        // SUPPRESSION
        val goneFishPos = moveFishResult.map(e => ((e._1.position.x, e._1.position.y), if(e.isInstanceOf[Tuna]) TUNA else SHARK)).diff(postFishMoveGrid.toList)

        // Récupération des poissons à supprimer
        val goneFishList = moveFishResult.map(e => e._1).filter(f =>
            goneFishPos.contains(((f.position.x, f.position.y), if(f.isInstanceOf[Tuna]) TUNA else SHARK))
        )

        // Listes finales
        val finalFishList = moveFishResult.map(e => e._1).diff(goneFishList) ::: newFishList

        copy(grid = postFishMoveGrid, fishList = finalFishList)
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
              fishGrid.get((pos.x + i, pos.y + j)) == Some(TUNA)

        } yield Coord(pos.x + i, pos.y + j)

        Random.shuffle(nearbyCells).headOption
    }

    private def moveShark(shark: Shark, newCoord: Option[Coord], sBreed: Int, sEnergy: Int, fishGrid: Map[(Int, Int), FishType]): (Shark, GridStep) = {
        // TODO : Vérifier l'utilité du param sEnergy
        if(shark.energy - 1 > 0) {
            // Déplacement
            newCoord match
                case Some(coord) =>
                    // Déplacement
                    if (shark.breed + 1 >= sBreed) {
                        // Déplacement avec reproduction
                        (shark.copy(coord, 0, shark.energy - 1), GridStep(REPRODUCE, shark.position, coord, SHARK))
                    }
                    else {
                        // Déplacement sans reproduction
                        (shark.copy(coord, shark.breed + 1, shark.energy - 1), GridStep(MOVE, shark.position, coord, SHARK))
                    }
                // Pas de déplacement
                case None => (shark, GridStep(NOTHING, shark.position, shark.position, SHARK))
        }
        else {
            // Décès
            println("Shark Death reported")
            (shark, GridStep(DIE, shark.position, shark.position, SHARK))
        }
    }

    /** Applique les changements concernant les thons stockés dans la gsList au map des positions */
    @tailrec
    private def getFishGrid(fishGrid: Map[(Int, Int), FishType], gsList: List[GridStep], gsIndex: Int): Map[(Int, Int), FishType] = {
        if(gsIndex < gsList.length) {
            val gs = gsList(gsIndex)

            gs.action match
                case MOVE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))
                      .updated((gs.newCoord.x, gs.newCoord.y), gs.fishType)

                    getFishGrid(newGrid, gsList, gsIndex + 1)
                case REPRODUCE =>
                    val newGrid = fishGrid
                      .updated((gs.newCoord.x, gs.newCoord.y), gs.fishType)

                    getFishGrid(newGrid, gsList, gsIndex + 1)
                case DIE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))

                    getFishGrid(newGrid, gsList, gsIndex + 1)
                case NOTHING =>
                    getFishGrid(fishGrid, gsList, gsIndex + 1)
        }
        else {
            fishGrid
        }
    }

    private def getFishTypeAtCoordinates(fishGrid: Map[(Int, Int), FishType], coord: Coord): Option[FishType] = {
        fishGrid.get((coord.x, coord.y))
    }
}