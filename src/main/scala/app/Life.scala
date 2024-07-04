package app

import app.FishType.{SHARK, TUNA}
import app.GridAction.{DIE, MOVE, NOTHING, REPRODUCE}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.annotation.tailrec
import scala.util.Random

final case class Life(grid : Map[(Int, Int), FishType], tunaList: List[Tuna], sharkList: List[Shark], agentSize: Int, gridBound: Int) {
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

        // Déplacement des thons
        val moveTunaResult: List[(Tuna, GridStep)] = tunaList.map(tuna =>
            getRandomFreeNearbyCellCoord(tuna.position) match
                case Some(newCoord) =>
                    // Déplacement du thon
                    if (tuna.breed + 1 >= tBreed) {
                        // Reproduction à notre ancienne position
                        println("Reproducing !")
                        (tuna.copy(position = newCoord, breed = 0), GridStep(REPRODUCE, tuna.position, newCoord))
                    }
                    else {
                        // Déplacement sans reproduction
                        (tuna.copy(position = newCoord, breed = tuna.breed + 1), GridStep(MOVE, tuna.position, newCoord))
                    }
                case None =>
                    // Pas de déplacement du thon
                    (tuna, GridStep(NOTHING, tuna.position, tuna.position))
        )
        
        val postTunaMoveGrid: Map[(Int, Int), FishType] = getTunaGrid(grid, moveTunaResult.map(e => e._2), 0)

        // SHARKS
        val moveSharkResult: List[(Shark, GridStep)] = sharkList.map(shark => {
            getNextTunaOccupiedCellCoord(shark.position) match
                case Some(c) =>
                    // Déplacement avec repas
                    val eatenTunaGrid = postTunaMoveGrid.removed((c.x, c.y))
                    moveShark(shark, Some(c), sBreed, sEnergy + 1, eatenTunaGrid)
                case None => getRandomFreeNearbyCellCoord(shark.position) match
                    // Déplacement sans repas
                    case Some(c) => moveShark(shark, Some(c), sBreed, sEnergy, postTunaMoveGrid)
                    // Pas de déplacement
                    case None => moveShark(shark, None, sBreed, sEnergy, postTunaMoveGrid)
        })
        
        val postSharkMoveGrid: Map[(Int, Int), FishType] = getSharkGrid(grid, moveSharkResult.map(e => e._2), 0)

        // LIMITE DE REFACTO - 04/07/2024 - 22:29

        // On crée les poissons dans la liste qui viennent d'être ajoutés à la grille
        val newTunas = for {
            i <- 0 until gridBound
            j <- 0 until gridBound if grid(i)(j).contains(TUNA) &&
                !updFishList.exists(f => f.position.x == i && f.position.y == j)
        } yield Tuna(Coord(i, j), 0)

        val newSharks = for {
            i <- 0 until gridBound
            j <- 0 until gridBound if grid(i)(j).contains(SHARK) &&
              !updFishList.exists(f => f.position.x == i && f.position.y == j)
        } yield Shark(Coord(i, j), 0, sEnergy)

        // => Chercher les poissons pour lesquels la position est à None dans la grille
        val deadFishes = updFishList.filter(f =>
            grid(f.position.x)(f.position.y).isEmpty
        )

        val finalFishList: List[Fish] = updFishList.diff(deadFishes) ::: newTunas.toList ::: newSharks.toList

        val finalGrid = finalFishList.map(f => ((f.position.x, f.position.y), )).toMap

        copy(grid = grid/*, fishList = finalFishList*/)
    }

    private def getRandomFreeNearbyCellCoord(pos: Coord): Option[Coord] = {
        val nearbyCells = for {
            i <- (-1 to 1).toList
            j <- (-1 to 1).toList if (i, j) != (0, 0) &&
              pos.x + i >= 0 && pos.y + j >= 0 &&
              pos.x + i < gridBound && pos.y + j < gridBound &&
              grid(pos.x + i)(pos.y + j).isEmpty

        } yield Coord(pos.x + i, pos.y + j)

        Random.shuffle(nearbyCells).headOption
    }

    private def getNextTunaOccupiedCellCoord(pos: Coord): Option[Coord] = {
        val nearbyCells = for {
            i <- (-1 to 1).toList
            j <- (-1 to 1).toList if (i, j) != (0, 0) &&
              pos.x + i >= 0 && pos.y + j >= 0 &&
              pos.x + i < gridBound && pos.y + j < gridBound &&
              grid(pos.x + i)(pos.y + j).contains(TUNA)

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
                        (shark.copy(coord, 0, shark.energy - 1), GridStep(REPRODUCE, shark.position, coord))
                    }
                    else {
                        // Déplacement sans reproduction
                        (shark.copy(coord, shark.breed + 1, shark.energy - 1), GridStep(MOVE, shark.position, coord))
                    }
                // Pas de déplacement
                case None => (shark, GridStep(NOTHING, shark.position, shark.position))
        }
        else {
            // Décès
            println("Shark Death reported")
            (shark, GridStep(DIE, shark.position, shark.position))
        }
    }

    /** Applique les changements concernant les thons stockés dans la gsList au map des positions */
    @tailrec
    private def getTunaGrid(fishGrid: Map[(Int, Int), FishType], gsList: List[GridStep], gsIndex: Int): Map[(Int, Int), FishType] = {
        if(gsIndex < gsList.length) {
            val gs = gsList(gsIndex)
            
            gs.action match
                case MOVE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))
                      .updated((gs.newCoord.x, gs.newCoord.y), TUNA)
                    
                    getTunaGrid(newGrid, gsList, gsIndex + 1)
                case REPRODUCE =>
                    val newGrid = fishGrid
                      .updated((gs.newCoord.x, gs.newCoord.y), TUNA)

                    getTunaGrid(newGrid, gsList, gsIndex + 1)
                case NOTHING =>
                    getTunaGrid(fishGrid, gsList, gsIndex + 1)
        }
        else {
            fishGrid
        }
    }

    /** Applique les changements concernant les requins stockés dans la gsList au map des positions */
    @tailrec
    private def getSharkGrid(fishGrid: Map[(Int, Int), FishType], gsList: List[GridStep], gsIndex: Int): Map[(Int, Int), FishType] = {
        if (gsIndex < gsList.length) {
            val gs = gsList(gsIndex)

            gs.action match
                case MOVE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))
                      .updated((gs.newCoord.x, gs.newCoord.y), TUNA)

                    getSharkGrid(newGrid, gsList, gsIndex + 1)
                case REPRODUCE =>
                    val newGrid = fishGrid
                      .updated((gs.newCoord.x, gs.newCoord.y), TUNA)

                    getSharkGrid(newGrid, gsList, gsIndex + 1)
                case DIE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))
                    
                    getSharkGrid(newGrid, gsList, gsIndex + 1)
                case NOTHING =>
                    getSharkGrid(fishGrid, gsList, gsIndex + 1)
        }
        else {
            fishGrid
        }
    }
}