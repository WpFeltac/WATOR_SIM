package app

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.{PrimaryStage, userAgentStylesheet_=}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.paint.Color.*
import scalafx.animation.KeyFrame
import scalafx.animation.Timeline
import scalafx.animation.Timeline.*
import scalafx.util.Duration
import scalafx.scene.paint.Color

import scala.util.Random

object Main extends JFXApp3 {

    private val windowSize = 600
    private val agentSize = 20

    private val nTunas = 1
    private val tBreed = 5

    private val nSharks = 0
    private val sBreed = 1000
    private val sEnergy = 1000

    private val gridBound = windowSize / agentSize

    override def start(): Unit = {

        // Initialisation des listes de poissons respectives
        val tunaList = List.fill(nTunas) {
            Tuna(
                Coord(Random.nextInt(gridBound), Random.nextInt(gridBound)),
                0
            )
        }
        println(tunaList.length + " tuna(s) spawned")

        val sharkList = List.fill(nSharks) {
            Shark(
                Coord(Random.nextInt(gridBound), Random.nextInt(gridBound)),
                0,
                sEnergy
            )
        }
        println(sharkList.length + " shark(s) spawned")        

        // Init HashMap
        val emptyGrid : Map[(Int, Int), FishType] = Map()
        val halfGrid = emptyGrid ++ tunaList.map(e => ((e.position.x, e.position.y), FishType.TUNA))
        val finalGrid = halfGrid ++ sharkList.map(e => ((e.position.x, e.position.y), FishType.SHARK))

        println(finalGrid)

        val life: ObjectProperty[Life] = ObjectProperty(Life(finalGrid, tunaList, sharkList, agentSize, gridBound))

        stage = new PrimaryStage {
            title = "WATOR Simulation"
            width = windowSize
            height = windowSize
            scene = new Scene {
                fill = White
                content = life.value.draw()
                life.onChange {
                    content = life.value.draw()
                }
            }
        }

        new Timeline {
            keyFrames = List(
                KeyFrame(
                    time = Duration(500),
                    onFinished = _ => life.update(life.value.move(tBreed, sBreed, sEnergy))
                )
            )
            cycleCount = Indefinite
        }.play()
    }
}


