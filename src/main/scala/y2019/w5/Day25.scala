package y2019.w5

import common.{Coords, Day}
import common.Utils._
import y2019.{AsciiInterface, Intcode}

import scala.collection.{mutable, immutable}

class Day25 extends Day(inputPath(2019, 25)) {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)

  private val stringDirections = Map("north" -> Coords.North, "south" -> Coords.South,
    "east" -> Coords.East, "west" -> Coords.West)
  private val directionStrings = stringDirections.map(_.swap)
  private val directionInstructions = stringDirections.keySet.map(_ + '\n')

  private val harmfulItems = Set("molten lava", "escape pod", "infinite loop", "photons", "giant electromagnet")
  private val instructionQueue = mutable.Queue.empty[String]
  private var currentCoords = Coords(0,0)
  private val coordsToSearch = mutable.Set.empty[Coords]
  private val searchedCoords = mutable.Map(currentCoords -> Set.empty[String])
  private var securityRoom: Option[Coords] = None
  private var psf: Option[Coords] = None
  private var directionFromSecurityRoom: String = ""
  private val currentRoute = mutable.Queue.empty[String]
  private val itemsToCheck = mutable.Set.empty[String]
  private val itemQueue = mutable.Queue.empty[String]
  private val positiveItems = mutable.Set.empty[String]
  private var (checking, eachChecked, checkingDropOne, dropOneChecked, checkingDropTwo, dropTwoChecked) =
    (false, false, false, false, false, false)

  private object Targets extends Enumeration {
    val Searching: Value = Value // find all harmless items
    val Going: Value = Value // go to the security room, when coordsToSearch contains only the coords of the PSF
    val Unlocking: Value = Value // find the combination, at the security room
  }
  private var target = Targets.Searching

  private val inventoryItems = mutable.Set.empty[String]
  private var password: Option[Int] = None

  private def findDoorsItems(outputString: String): (Array[String], Array[String]) = {
    val paragraphs = outputString.split("\n\n")
    val doors = paragraphs.find(_.startsWith("Doors here lead:\n")) match {
      case None => Array.empty[String]
      case Some(s) => s.split('\n').tail.map(_.drop(2))
    }
    val items = paragraphs.find(_.startsWith("Items here:\n")) match {
      case None => Array.empty[String]
      case Some(s) => s.split('\n').tail.map(_.drop(2))
    }
    (doors, items)
  }

  private def findPassword(outputString: String): Int = {
    val paragraphs = outputString.trim.split("\n\n")
    val sentences = paragraphs(2).split('\n')
    val words = sentences(2).split(' ')
    words(11).toInt
  }

  case class SearchUnit(coords: Coords, route: immutable.Queue[Coords])

  private def getMinRoute(start: Coords, ends: Set[Coords]): immutable.Queue[String] = {
    val possibleAdjacent = ends.find(end =>
      directionStrings.contains(end - start) && searchedCoords(start).contains(directionStrings(end - start)))
    if (possibleAdjacent.nonEmpty)
      immutable.Queue(directionStrings(possibleAdjacent.get - start))
    else {
      val searchQueue = mutable.Queue(SearchUnit(start, immutable.Queue.empty[Coords]))
      val searched = mutable.Set(start)

      do {
        val currentUnit = searchQueue.dequeue()
        if (ends.contains(currentUnit.coords)) return currentUnit.route.map(directionStrings)

        val possibleNextCoords = searchedCoords(currentUnit.coords).map(stringDirections(_) + currentUnit.coords)
          .diff(searched)
        val possibleNextUnits =
          (if (securityRoom.isEmpty || target == Targets.Going) possibleNextCoords else possibleNextCoords.filterNot(_ == psf.get))
            .map(coords => {
              searched.add(coords)
              SearchUnit(coords, currentUnit.route.enqueue(coords - currentUnit.coords))
            })
        searchQueue.enqueueAll(possibleNextUnits)
      } while (searchQueue.nonEmpty)
      immutable.Queue.empty
    }
  }

  private def nextDirectionInstruction(doors: Set[String]): String = {
    if (currentRoute.isEmpty) {
      val doorCoords = doors.map(stringDirections(_) + currentCoords)
      coordsToSearch.addAll(doorCoords.filterNot(searchedCoords.contains))
      val coordsToGoTo =
        if (target == Targets.Searching && coordsToSearch.size == 1 &&
          securityRoom.nonEmpty && psf.get == coordsToSearch.head) {
          target = Targets.Going
          Set(psf.get)
        } else if (securityRoom.isEmpty) coordsToSearch.toSet
        else coordsToSearch.toSet.excl(psf.get)
      currentRoute.enqueueAll(getMinRoute(currentCoords, coordsToGoTo))
    }
    currentRoute.dequeue() + '\n'
  }

  private def runOneStep(program: Intcode, input: String = ""): Unit = {
    val outputs = mutable.ArrayBuffer(0L)
    if (input != "") AsciiInterface.inputString(program, outputs, input)
    else program.run(outputs)

    if (directionInstructions.contains(input)) {
      currentCoords = currentCoords + stringDirections(input.init)
      coordsToSearch.remove(currentCoords)
      searchedCoords.addOne(currentCoords, Set.empty[String])
    } else if (input.startsWith("take ")) inventoryItems.add(input.drop(5).init)
    else if (input.startsWith("drop ")) inventoryItems.remove(input.drop(5).init)

    val outputString = AsciiInterface.outputs2String(outputs.tail)
    print(outputString)

    if (!outputString.startsWith("\n\n\n== Security Checkpoint ==") &&
      !outputString.startsWith("\n\n\n== Pressure-Sensitive Floor ==") &&
      !outputString.contains("inventory") && !outputString.contains("carrying")) {
      if (!outputString.startsWith("\nYou ")) {
        if (searchedCoords(currentCoords).isEmpty) {
          var (doors, items) = findDoorsItems(outputString)
          if (items.length > 0)
            instructionQueue.enqueueAll(items.filterNot(harmfulItems.contains).map(item => s"take $item\n"))
          if (currentCoords == Coords(0,0))
            doors = doors.toSet.excl("east").toArray
          searchedCoords.update(currentCoords, doors.toSet)
        }
        instructionQueue.enqueue(nextDirectionInstruction(searchedCoords(currentCoords)))
      }
    } else if (outputString.contains("verify your identity") && !outputString.contains("Alert!")) {
      if (securityRoom.isEmpty) {
        securityRoom = Some(currentCoords)
        val (doors, _) = findDoorsItems(outputString)
        searchedCoords.update(currentCoords, doors.toSet)
        directionFromSecurityRoom = doors
          .find(door => !searchedCoords.contains(currentCoords + stringDirections(door)))
          .get
        psf = Some(securityRoom.get + stringDirections(directionFromSecurityRoom))
      }
      instructionQueue.enqueue(nextDirectionInstruction(searchedCoords(currentCoords)))
    } else if (outputString.contains("Alert!") ||
      outputString.contains("inventory") || outputString.contains("carrying")) {
      if (!checking) {
        itemsToCheck.addAll(inventoryItems)
        itemQueue.enqueueAll(itemsToCheck)
        instructionQueue.enqueueAll(itemsToCheck.map(item => s"drop $item\n"))
        instructionQueue.enqueueAll(Array(
          s"take ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        checking = true
      } else if (!eachChecked) {
        val itemBeingChecked = itemQueue.dequeue()
        if (outputString.contains("lighter")) itemsToCheck.remove(itemBeingChecked)
        instructionQueue.enqueue(s"drop $itemBeingChecked\n")
        if (itemQueue.nonEmpty) instructionQueue.enqueueAll(Array(
          s"take ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        else {
          instructionQueue.enqueue("inv\n")
          eachChecked = true
        }
      } else if (!checkingDropOne) {
        itemQueue.enqueueAll(itemsToCheck)
        instructionQueue.enqueueAll(itemsToCheck.map(item => s"take $item\n"))
        instructionQueue.enqueueAll(Array(
          s"drop ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        checkingDropOne = true
      } else if (!dropOneChecked) {
        val itemBeingChecked = itemQueue.dequeue()
        if (outputString.contains("heavier")) {
          itemsToCheck.remove(itemBeingChecked)
          positiveItems.add(itemBeingChecked)
        }
        instructionQueue.enqueue(s"take $itemBeingChecked\n")
        if (itemQueue.nonEmpty) instructionQueue.enqueueAll(Array(
          s"drop ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        else {
          instructionQueue.enqueue("inv\n")
          dropOneChecked = true
        }
      } else if (!checkingDropTwo) {
        // theoretically more should be checked but let's just stop here
        itemQueue.enqueueAll(itemsToCheck)
        instructionQueue.enqueueAll(itemsToCheck.map(item => s"drop $item\n"))
        instructionQueue.enqueueAll(Array(
          s"take ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        checkingDropTwo = true
      } else if (!dropTwoChecked) {
        val itemBeingChecked = itemQueue.dequeue()
        if (outputString.contains("lighter")) itemsToCheck.remove(itemBeingChecked)
        instructionQueue.enqueue(s"drop $itemBeingChecked\n")
        if (itemQueue.nonEmpty) instructionQueue.enqueueAll(Array(
          s"take ${itemQueue.head}\n", s"$directionFromSecurityRoom\n"))
        else {
          instructionQueue.enqueue("inv\n")
          dropTwoChecked = true
        }
      } else println("Not too bad!")
    } else {
      password = Some(findPassword(outputString))
    }

    if (target == Targets.Searching && coordsToSearch.size == 1 &&
      securityRoom.nonEmpty && psf.get == coordsToSearch.head) target = Targets.Going
    else if (target == Targets.Going && currentCoords == securityRoom.get) target = Targets.Unlocking
  }

  private def run(program: Intcode): Int = {
    while (password.isEmpty) {
      if (instructionQueue.isEmpty) runOneStep(program)
      else runOneStep(program, instructionQueue.dequeue())
    }
    password.get
  }

  /*private def run(): Unit = {
    val program = Intcode(codes)

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
  }
  run()*/

  def one: Int = run(Intcode(codes))

  def two: Unit = {}
}