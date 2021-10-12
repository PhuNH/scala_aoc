package y2019.w5

import common.{Coords, Day}
import common.Utils._
import y2019.{AsciiInterface, Intcode}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day25 extends Day(inputPath(2019, 25)) {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)
  private val availableCoords = mutable.Set.empty[Coords]
  private val visitedCoords = mutable.Set.empty[Coords]
  private val currentCoords = Coords(0,0)
  visitedCoords.add(currentCoords.clone)
  private val instructionQueue = mutable.Queue.empty[String]
  private val itemsToSkip = Set("molten lava", "escape pod", "infinite loop", "photons", "giant electromagnet")
  private val directionInstructions = Set("north\n", "south\n", "west\n", "east\n")
  private val securityRoom = Coords.unknown
  private var directionFromSecurityRoom = ""
  private var weightState = 0
  private val triedCombinations = mutable.Map.empty[Set[String], Int]
  private val combinationsToTry = mutable.Set.empty[Seq[String]]
  private val inventoryItems = mutable.Set.empty[String]

  private def parseOutputs(outputString: String): (Array[String], Array[String]) = {
    val paragraphs = outputString.split("\n\n")
    val leads = paragraphs.find(_.startsWith("Doors here lead:\n")) match {
      case None => Array.empty[String]
      case Some(s) => s.split('\n').tail.map(_.drop(2))
    }
    val items = paragraphs.find(_.startsWith("Items here:\n")) match {
      case None => Array.empty[String]
      case Some(s) => s.split('\n').tail.map(_.drop(2))
    }
    (leads, items)
  }

  private def runOneStep(program: Intcode, input: String = ""): Unit = {
    val outputs = ArrayBuffer(0L)
    if (input != "") {
      AsciiInterface.inputString(program, outputs, input)
      if (directionInstructions.contains(input)) {
        if (input == "north\n") currentCoords += Coords.North
        else if (input == "south\n") currentCoords += Coords.South
        else if (input == "west\n") currentCoords += Coords.West
        else currentCoords += Coords.East
        availableCoords.remove(currentCoords)
        visitedCoords.add(currentCoords.clone)
      } else if (input.startsWith("take ")) {
        inventoryItems.add(input.drop(5).init)
      } else if (input.startsWith("drop ")) {
        inventoryItems.remove(input.drop(5).init)
      }
    }
    else program.run(outputs)
//    Thread.sleep(1000)

    val outputString = AsciiInterface.outputs2String(outputs.tail)
    if (outputString.contains("verify your identity") && !outputString.contains("Alert!")) {
      securityRoom.updateWith(currentCoords)
      if (directionFromSecurityRoom == "") {
        val (leads, _) = parseOutputs(outputString)
        val nextDirections = leads.map {
          case "north" => (Coords.North, "north")
          case "south" => (Coords.South, "south")
          case "east" => (Coords.East, "east")
          case _ => (Coords.West, "west")
        }
        val theDirection = nextDirections.find(direction => !visitedCoords.contains(currentCoords + direction._1)).get
        directionFromSecurityRoom = theDirection._2
      }
      val command = directionFromSecurityRoom + '\n'
      instructionQueue.enqueue(command)
    } else if (outputString.contains("Alert!")) {
      if (outputString.contains("lighter")) weightState = 1
      else if (outputString.contains("heavier")) weightState = -1
      if (weightState != 0) {
        currentCoords.updateWith(securityRoom)
//        instructionQueue.enqueue("inv\n")
        triedCombinations.addOne(inventoryItems.toSet, weightState)
        if (weightState > 0) {
          combinationsToTry.addAll(inventoryItems.toSeq.combinations(inventoryItems.size-1))
          val firstToTry = combinationsToTry.find(c => !triedCombinations.contains(c.toSet)).get
          combinationsToTry.remove(firstToTry)
          val toDrop = inventoryItems.diff(firstToTry.toSet)
          instructionQueue.enqueue(s"drop ${toDrop.head}\n")
        }
        instructionQueue.enqueue(directionFromSecurityRoom + '\n')
      }
//    } else if (outputString.contains("Items in your inventory")) {
//      val paragraphs = outputString.split("\n\n")
//      val inventoryItems = paragraphs.find(_.startsWith("Items in your inventory")).get.split('\n').tail.map(_.drop(2)).toSet
    } else {
      val (leads, items) = parseOutputs(outputString)
      if (items.length > 0)
        instructionQueue.enqueueAll(items.filterNot(itemsToSkip.contains).map(item => s"take $item\n"))
      if (leads.length > 0) {
        val nextCoords = leads.map(lead => (lead match {
          case "north" => Coords.North
          case "south" => Coords.South
          case "east" => Coords.East
          case _ => Coords.West
        }) + currentCoords)
        availableCoords.addAll(nextCoords.filterNot(visitedCoords.contains))

        val target =
          if (availableCoords.isEmpty) securityRoom
          else availableCoords.minBy(currentCoords.mhtDistWith)
        val closest = nextCoords.minBy(target.mhtDistWith)
        val command = {
          if (closest == currentCoords.north) "north\n"
          else if (closest == currentCoords.south) "south\n"
          else if (closest == currentCoords.east) "east\n"
          else "west\n"
        }
        instructionQueue.enqueue(command)
      }
    }
    print(outputString)
  }

  private def run(): Unit = {
    val program = Intcode(codes)
//    while (true) {
//      if (instructionQueue.isEmpty) runOneStep(program)
//      else runOneStep(program, instructionQueue.dequeue())
      runOneStep(program, "north\n")
      runOneStep(program, "take easter egg\n")
      runOneStep(program, "east\n")
      runOneStep(program, "take astrolabe\n")
      runOneStep(program, "south\n")
      runOneStep(program, "take space law space brochure\n")
      runOneStep(program, "north\n")
      runOneStep(program, "north\n")
      runOneStep(program, "north\n")
      runOneStep(program, "take fuel cell\n")
      runOneStep(program, "south\n")
      runOneStep(program, "south\n")
      runOneStep(program, "west\n")
      runOneStep(program, "north\n")
      runOneStep(program, "north\n")
      runOneStep(program, "north\n")
      runOneStep(program, "north\n")
      runOneStep(program, "take weather machine\n")
      runOneStep(program, "north\n")
      runOneStep(program, "take antenna\n")
      runOneStep(program, "west\n")

    runOneStep(program, "drop antenna\n")
    runOneStep(program, "drop easter egg\n")
    runOneStep(program, "south\n")
    runOneStep(program, "take easter egg\n")
    runOneStep(program, "drop fuel cell\n")
    runOneStep(program, "south\n")
    runOneStep(program, "take fuel cell\n")
    runOneStep(program, "take antenna\n")

    runOneStep(program, "drop easter egg\n")
    runOneStep(program, "drop fuel cell\n")
    runOneStep(program, "south\n")
    runOneStep(program, "take fuel cell\n")
    runOneStep(program, "take easter egg\n")

//      runOneStep(program, "south\n")
//      runOneStep(program, "drop weather machine\n")
//      runOneStep(program, "take fuel cell\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "drop fuel cell\n")
//      runOneStep(program, "take space law space brochure\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "drop space law space brochure\n")
//      runOneStep(program, "take easter egg\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "take space law space brochure\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "drop space law space brochure\n")
//      runOneStep(program, "take fuel cell\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "drop fuel cell\n")
//      runOneStep(program, "take weather machine\n")
//      runOneStep(program, "south\n")
//      runOneStep(program, "drop weather machine\n")
//      runOneStep(program, "take antenna\n")
//      runOneStep(program, "south\n")
//    }
  }
  run()

  def one: Unit = {}

  def two: Unit = {}
}