package app

import app.FishType.{SHARK, TUNA}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.util.Random

final case class Life(grid : Map[(Int, Int), FishType], tunaList: List[Tuna], sharkList: List[Shark], agentSize: Int, gridBound: Int) {
    def draw(): List[Rectangle] = {
        // On peuple les grilles d'après les listes des poissons passées
        val tunaGrid = tunaList.map(e => {
            (e.position.x, e.position.y) -> FishType.TUNA
        }).toMap

        val sharkGrid = sharkList.map(e => {
            (e.position.x, e.position.y) -> FishType.SHARK
        }).toMap

        // On génère les rectangles à dessiner aux coordonnées correspondantes
        // d'après les grids (maps)
        val tunaShapeList = tunaGrid.map(e =>
            new Rectangle {
                x = e._1._1 * agentSize
                y = e._1._2 * agentSize
                width = agentSize
                height = agentSize
                fill = Color.rgb(189, 105, 76)
            }
        ).toList
        
        val sharkShapeList = sharkGrid.map(e =>
            new Rectangle {
                x = e._1._1 * agentSize
                y = e._1._2 * agentSize
                width = agentSize
                height = agentSize
                fill = Color.rgb(42, 72, 99)
            }
        ).toList       
        
        // On renvoie la liste des rectangles à dessiner
        tunaShapeList ::: sharkShapeList
    }

    def move(tBreed: Int, sBreed: Int, sEnergy: Int): Life = {
        // On initalise la position de tous les poissons dans un map
        val fishGrid = tunaList.map(e => ((e.position.x, e.position.y), FishType.TUNA)).toMap          
            ++ sharkList.map(e => ((e.position.x, e.position.y), FishType.SHARK)).toMap
        
        // Déplacement des thons
        val moveTunaList = tunaList.map(tuna => {
            getRandomFreeNearbyCellCoord(tuna.position) match
                case Some(c) =>
                    
                    grid + ((c.x, c.y) -> Some(TUNA))
                    // Gestion reproduction
                    if (tuna.breed + 1 >= tBreed) {
                        // On ajoute un thon à notre ancienne position
                        grid + ((tuna.position.x, tuna.position.y) -> Some(TUNA))
                        tuna.copy(position = c, breed = 0)
                    }
                    // Pas de place libre
                    else {
                        grid(tuna.position.x)(tuna.position.y) = None
                        tuna.copy(position = c, breed = tuna.breed + 1)
                    }

                case None =>
                    tuna
        })

        val updFishList = randomFishList.map {
            case tuna: Tuna =>
                // On cherche une place libre autour de lui
                getRandomFreeNearbyCellCoord(tuna.position) match
                    case Some(c) =>
                        grid + ((c.x, c.y) -> Some(TUNA))                        
                        // Gestion reproduction
                        if (tuna.breed + 1 >= tBreed) {
                            // On ajoute un thon à notre ancienne position
                            grid + ((tuna.position.x, tuna.position.y) -> Some(TUNA))
                            tuna.copy(position = c, breed = 0)
                        }
                        // Pas de place libre
                        else {
                            grid(tuna.position.x)(tuna.position.y) = None
                            tuna.copy(position = c, breed = tuna.breed + 1)
                        }

                    case None =>
                        tuna

            case shark: Shark =>
                getNextTunaOccupiedCellCoord(shark.position) match
                    case Some(c) =>
                        // on retire le thon que l'on mange
                        grid(c.x)(c.y) = None
                        moveShark(shark, c, sBreed, sEnergy + 1)
                    case None => getRandomFreeNearbyCellCoord(shark.position) match
                        case Some(c) => moveShark(shark, c, sBreed, sEnergy)
                        case None => shark
        }

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

        val finalFishList = updFishList.diff(deadFishes) ::: newTunas.toList ::: newSharks.toList

        //println(finalFishList)

        copy(grid = grid, fishList = finalFishList)
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

    private def moveShark(shark: Shark, newCoord: Coord, sBreed: Int, sEnergy: Int): Shark = {

        // TODO : checker le bon déplacement du requin avec le None dans la grille
        // Gestion énergie
        if(shark.energy - 1 > 0) {
            grid(newCoord.x)(newCoord.y) = Some(SHARK)

            // Gestion reproduction
            if (shark.breed + 1 >= sBreed) {
                // On ajoute un requin à notre ancienne position
                grid(shark.position.x)(shark.position.y) = Some(SHARK)
                shark.copy(newCoord, 0, shark.energy - 1)
            }
            else {
                shark.copy(newCoord, shark.breed + 1, shark.energy - 1)
            }
        }
        else {
            // Le requin meurt
            grid(shark.position.x)(shark.position.y) = None
            println("Shark Death reported")
            shark
        }
    }
}