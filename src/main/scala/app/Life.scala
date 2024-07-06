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
        val moveFishResult: (Map[(Int, Int), FishType], List[Fish]) = handleFishMovement(grid, fishList, 0, tBreed, sBreed)

        // TODO : tout revoir à la suite de cette ligne
        /** Décrit la grille finale suite à tous les mouvements de poissons */
        val postFishMoveGrid = moveFishResult._1
        val postFishMoveList = moveFishResult._2

        // Gestion les poissons qui :
        // - sont dans la grille mais pas dans la liste (issus de reproduction = ajout)
        // - sont dans la liste mais plus dans la grille (issus de mouvement ou de mort = suppression)

        // AJOUT
        // Récupération des positions nouvelles par rapport à la grille de base
        val newFishPos = postFishMoveGrid.toList.diff(postFishMoveList.map(f => ((f.position.x, f.position.y), if(f.isInstanceOf[Tuna]) TUNA else SHARK)))
        // Création des poissons par rapport à ces positions
        val newFishList = newFishPos.map(pos =>
            pos._2 match
                case TUNA =>
                    Tuna(Coord(pos._1._1, pos._1._2), 0)
                case SHARK =>
                    Shark(Coord(pos._1._1, pos._1._2), 0, sEnergy)

        )

        // SUPPRESSION
        val goneFishPos = postFishMoveList.map(f => ((f.position.x, f.position.y), if(f.isInstanceOf[Tuna]) TUNA else SHARK)).diff(postFishMoveGrid.toList)
        // Récupération des poissons à supprimer
        val goneFishList = postFishMoveList.filter(f =>
            goneFishPos.contains(((f.position.x, f.position.y), if(f.isInstanceOf[Tuna]) TUNA else SHARK))
        )

        // Liste finale
        val finalFishList = postFishMoveList.diff(goneFishList) ::: newFishList

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
              fishGrid.get((pos.x + i, pos.y + j)).contains(TUNA)

        } yield Coord(pos.x + i, pos.y + j)

        Random.shuffle(nearbyCells).headOption
    }

    private def moveShark(fishList: List[Fish], shark: Shark, newCoord: Option[Coord], sBreed: Int, fishGrid: Map[(Int, Int), FishType]): (Map[(Int, Int), FishType], List[Fish]) = {
        if(shark.energy - 1 > 0) {
            // Déplacement
            newCoord match
                case Some(coord) =>
                    // Déplacement
                    if (shark.breed + 1 >= sBreed) {
                        // Déplacement avec reproduction
                        val updatedList = fishList.filterNot(f => f == shark) :+ Shark(coord, 0, shark.energy - 1)
                        val updatedGrid = getFishGrid(fishGrid, GridStep(REPRODUCE, shark.position, coord, SHARK))
                        (updatedGrid, updatedList)
                    }
                    else {
                        // Déplacement sans reproduction
                        val updatedList = fishList.filterNot(f => f == shark) :+ Shark(coord, shark.breed + 1, shark.energy - 1)
                        val updatedGrid = getFishGrid(fishGrid, GridStep(MOVE, shark.position, coord, SHARK))
                        (updatedGrid, updatedList)
                    }
                // Pas de déplacement
                case None =>
                    (fishGrid, fishList)
        }
        else {
            // Décès
            println("Shark Death reported")
            val updatedGrid = getFishGrid(fishGrid, GridStep(DIE, shark.position, shark.position, SHARK))
            (updatedGrid, fishList)
        }
    }
    
    private def getFishGrid(fishGrid: Map[(Int, Int), FishType], gs: GridStep): Map[(Int, Int), FishType] = {
        gs.action match
            case MOVE =>
                fishGrid
                  .removed((gs.oldCoord.x, gs.oldCoord.y))
                  .updated((gs.newCoord.x, gs.newCoord.y), gs.fishType)                
            case REPRODUCE =>
                fishGrid
                  .updated((gs.newCoord.x, gs.newCoord.y), gs.fishType)
            case DIE =>
                fishGrid
                  .removed((gs.oldCoord.x, gs.oldCoord.y))
            case NOTHING =>
                fishGrid
    }

    @tailrec
    private def handleFishMovement(fishGrid: Map[(Int, Int), FishType], fishList: List[Fish], index: Int, tBreed: Int, sBreed: Int): (Map[(Int, Int), FishType], List[Fish]) = {
        if(index < fishList.length) {
            fishList(index) match
                case fish: Tuna =>
                    getRandomFreeNearbyCellCoord(fishGrid, fish.position) match
                        case Some(newCoord) =>
                            // Déplacement du thon
                            if (fish.breed + 1 >= tBreed) {
                                // Reproduction à notre ancienne position
                                val updatedList = fishList.filterNot(f => f == fish) :+ Tuna(newCoord, 0)
                                val updatedGrid = getFishGrid(fishGrid, GridStep(REPRODUCE, fish.position, newCoord, TUNA))
                                handleFishMovement(updatedGrid, updatedList, index + 1, tBreed, sBreed)
                            }
                            else {
                                // Déplacement sans reproduction
                                val updatedList = fishList.filterNot(f => f == fish) :+ Tuna(newCoord, fish.breed + 1)
                                val updatedGrid = getFishGrid(fishGrid, GridStep(MOVE, fish.position, newCoord, TUNA))
                                handleFishMovement(updatedGrid, updatedList, index + 1, tBreed, sBreed)
                            }
                        case None =>
                            // Pas de déplacement du thon
                            handleFishMovement(fishGrid, fishList, index + 1, tBreed, sBreed)
                case fish: Shark =>
                    getNextTunaOccupiedCellCoord(fishGrid, fish.position) match
                        case Some(c) =>
                            // Déplacement avec repas
                            val eatenTunaGrid = fishGrid.removed((c.x, c.y))
                            val sharkMoveResult = moveShark(fishList, fish.copy(energy = fish.energy + 1), Some(c), sBreed, eatenTunaGrid)
                            handleFishMovement(sharkMoveResult._1, sharkMoveResult._2, index + 1, tBreed, sBreed)                            
                        case None =>
                            getRandomFreeNearbyCellCoord(fishGrid, fish.position) match
                                // Déplacement sans repas
                                case Some(c) =>
                                    val sharkMoveResult = moveShark(fishList, fish, Some(c), sBreed, fishGrid)
                                    handleFishMovement(sharkMoveResult._1, sharkMoveResult._2, index + 1, tBreed, sBreed)
                                // Pas de déplacement
                                case None =>
                                    val sharkMoveResult = moveShark(fishList, fish, None, sBreed, fishGrid)
                                    handleFishMovement(sharkMoveResult._1, sharkMoveResult._2, index + 1, tBreed, sBreed)
        }
        else {
            (fishGrid, fishList)
        }
    }
}