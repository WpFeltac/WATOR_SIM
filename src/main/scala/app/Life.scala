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
        // TUNAS
        val moveTunaResult: List[(Tuna, GridStep)] = tunaList.map(tuna =>
            getRandomFreeNearbyCellCoord(grid, tuna.position) match
                case Some(newCoord) =>
                    // Déplacement du thon
                    if (tuna.breed + 1 >= tBreed) {
                        // Reproduction à notre ancienne position
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
            getNextTunaOccupiedCellCoord(postTunaMoveGrid, shark.position) match
                case Some(c) =>
                    // Déplacement avec repas
                    val eatenTunaGrid = postTunaMoveGrid.removed((c.x, c.y))
                    moveShark(shark, Some(c), sBreed, sEnergy + 1, eatenTunaGrid)
                case None => getRandomFreeNearbyCellCoord(postTunaMoveGrid, shark.position) match
                    // Déplacement sans repas
                    case Some(c) =>
                        moveShark(shark, Some(c), sBreed, sEnergy, postTunaMoveGrid)
                    // Pas de déplacement
                    case None => moveShark(shark, None, sBreed, sEnergy, postTunaMoveGrid)
        })

        /** Décrit la grille finale suite à tous les mouvements de poissons */
        // /!\ IMPORTANT : on lui passe la grille ayant déjà traité les mouvements des thons
        val postFishMoveGrid: Map[(Int, Int), FishType] = getSharkGrid(postTunaMoveGrid, moveSharkResult.map(e => e._2), 0)

        val postFishMoveList: List[Fish] = moveTunaResult.map(e => e._1) ::: moveSharkResult.map(e => e._1)

        // Gestion les poissons qui :
        // - sont dans la grille mais pas dans la liste (issus de reproduction = ajout)
        // - sont dans la liste mais plus dans la grille (issus de mouvement ou de mort = suppression)

        // AJOUT
        // Récupération des positions nouvelles par rapport à la grille de base
        val newTunasPos = postTunaMoveGrid.toList.diff(moveTunaResult.map(e => ((e._1.position.x, e._1.position.y), TUNA)))
        val newSharksPos = postFishMoveGrid.filter(g => g._2 == SHARK).toList.diff(moveSharkResult.map(e => ((e._1.position.x, e._1.position.y), SHARK)))

        // Création des poissons par rapport à ces positions
        val newTunasList = newTunasPos.map(pos =>
            Tuna(Coord(pos._1._1, pos._1._2), 0)
        )
        val newSharksList = newSharksPos.map(pos =>
            Shark(Coord(pos._1._1, pos._1._2), 0, sEnergy)
        )

        // SUPPRESSION
        val goneTunasPos = moveTunaResult.map(e => ((e._1.position.x, e._1.position.y), TUNA)).diff(postTunaMoveGrid.toList)
        val goneSharksPos = moveSharkResult.map(e => ((e._1.position.x, e._1.position.y), SHARK)).diff(postFishMoveGrid.toList)

        // Récupération des poissons à supprimer
        val goneTunasList = moveTunaResult.map(e => e._1).filter(t =>
            goneTunasPos.contains(((t.position.x, t.position.y), TUNA))
        )
        val goneSharksList = moveSharkResult.map(e => e._1).filter(s =>
            goneSharksPos.contains(((s.position.x, s.position.y), SHARK))
        )

        // Listes finales
        val finalTunaList = moveTunaResult.map(e => e._1).diff(goneTunasList) ::: newTunasList
        val finalSharkList = moveSharkResult.map(e => e._1).diff(goneSharksList) ::: newSharksList

        copy(grid = postFishMoveGrid, tunaList = finalTunaList, sharkList = finalSharkList)
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
            fishGrid.filter(p => p._2 == TUNA)
        }
    }

    /** Applique les changements concernant les requins stockés dans la gsList au map des positions */
    @tailrec
    private def getSharkGrid(fishGrid: Map[(Int, Int), FishType], gsList: List[GridStep], gsIndex: Int): Map[(Int, Int), FishType] = {
        // TODO : gérer le thon mangé
        if (gsIndex < gsList.length) {
            val gs = gsList(gsIndex)

            gs.action match
                case MOVE =>
                    val newGrid = fishGrid
                      .removed((gs.oldCoord.x, gs.oldCoord.y))
                      .updated((gs.newCoord.x, gs.newCoord.y), SHARK)

                    getSharkGrid(newGrid, gsList, gsIndex + 1)
                case REPRODUCE =>
                    val newGrid = fishGrid
                      .updated((gs.newCoord.x, gs.newCoord.y), SHARK)

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