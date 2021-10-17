package y2018.w5

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day24 extends Day(inputPath(2018, 24)) {
  private val armyDescriptions = using(Source.fromResource(inputs(0)))(
    _.getLines().mkString("\n").split("\n\n").map(_.split('\n')))

  private case class GroupInfo(hp: Int, atkDam: Int, atkType: String, init: Int,
                               weak: Set[String], immune: Set[String])
  
  private case class Group(var units: Int, info: GroupInfo, var boost: Int = 0)
    extends Ordered[Group] with mutable.Cloneable[Group] {
    override def compare(that: Group): Int = {
      if (this.effPow != that.effPow) this.effPow - that.effPow
      else this.info.init - that.info.init
    }

    override def clone(): Group = Group(units, info, boost)

    def effPow: Int = units * (info.atkDam + boost)
    
    def selectTarget(enemy: Army, selected: mutable.Set[Int]): Option[Int] = {
      val damageable = enemy.groups
        .filterNot { case (index, group) => selected.contains(index) || group.info.immune.contains(info.atkType) }
      if (damageable.isEmpty) None
      else {
        val (double, single) = damageable.partition(_._2.info.weak.contains(info.atkType))
        if (double.nonEmpty) Some(double.toArray.maxBy(_._2)._1)
        else Some(single.toArray.maxBy(_._2)._1)
      }
    }

    def attack(enemy: Army, targetIndex: Int): Unit = {
      val factor = if (enemy.groups(targetIndex).info.weak.contains(info.atkType)) 2 else 1
      val lost = effPow * factor / enemy.groups(targetIndex).info.hp
      val left = enemy.groups(targetIndex).units - lost
      if (left <= 0) enemy.groups.remove(targetIndex)
      else enemy.groups(targetIndex).units -= lost
    }
  }

  private case class TargetSelection(side: Int, index: Int, init: Int, targetIndex: Int)
  
  private case class Army(var groups: mutable.Map[Int, Group], side: Int) extends mutable.Cloneable[Army] {
    def selectTarget(enemy: Army): Array[TargetSelection] = {
      val targetMap = mutable.Map.empty[Int, Int]
      val selected = mutable.Set.empty[Int]
      groups.toArray.sortBy(_._2).reverse.foreach { case (srcIndex, srcGroup) =>
        val targetIndex = srcGroup.selectTarget(enemy, selected)
        if (targetIndex.nonEmpty) {
          selected.add(targetIndex.get)
          targetMap.addOne(srcIndex, targetIndex.get)
        }
      }
      targetMap.toArray.map { case (srcIndex, targetIndex) =>
        TargetSelection(side, srcIndex, groups(srcIndex).info.init, targetIndex) }
    }

    override def clone(): Army = {
      val clonedGroups = groups.map { case (index, group) => (index, group.clone())}
      Army(clonedGroups, side)
    }

    def boost(boostValue: Int): Unit = {
      groups.values.foreach(g => g.boost = boostValue)
    }

    def units: Int = groups.map(_._2.units).sum
  }

  private object Army {
    private def extractImmuneWeak(desc: String, side: Int): Set[String] =
      desc.substring(if (side == 0) 10 else 8).split(',').map(_.trim).toSet

    def apply(desc: Array[String]): Army = {
      val side = if (desc(0).contains("Immune")) 0 else 1
      val groups = mutable.Map.from(desc.tail.zipWithIndex.map { case (l, index) =>
        val words = l.split(' ')
        val (units, hp) = (words(0).toInt, words(4).toInt)
        val parts = l.split(Array('(', ')'))
        val (atkDam, atkType, init) =
          if (parts.length == 1) (words(12).toInt, words(13), words(17).toInt)
          else {
            val partWords = parts(2).trim.split(' ')
            (partWords(5).toInt, partWords(6), partWords(10).toInt)
          }
        val (weak, immune) =
          if (parts.length == 1) (Set.empty[String], Set.empty[String])
          else {
            var (insideWeak, insideImmune) = (Set.empty[String], Set.empty[String])
            if (parts(1).contains("immune") && parts(1).contains("weak")) {
              val subParts = parts(1).split(';').map(_.trim)
              for (sp <- subParts) {
                if (sp.startsWith("immune")) insideImmune = extractImmuneWeak(sp, 0)
                else insideWeak = extractImmuneWeak(sp, 1)
              }
            } else {
              if (parts(1).startsWith("immune")) insideImmune = extractImmuneWeak(parts(1), 0)
              else insideWeak = extractImmuneWeak(parts(1), 1)
            }
            (insideWeak, insideImmune)
          }
        (index, Group(units, GroupInfo(hp, atkDam, atkType, init, weak, immune)))
      })
      Army(groups, side)
    }
  }

  private val armies = armyDescriptions.map(Army.apply)
  private val immune = armies.find(_.side == 0).get
  private val infection = armies.find(_.side == 1).get

  private case class Battle(immune: Army, infection: Army) {
    private def nextAttack(attackQueue: mutable.Queue[TargetSelection]): Unit = {
      val attack = attackQueue.dequeue()
      if (attack.side == 0 && immune.groups.contains(attack.index) && infection.groups.contains(attack.targetIndex))
        immune.groups(attack.index).attack(infection, attack.targetIndex)
      else if (attack.side == 1 && infection.groups.contains(attack.index) && immune.groups.contains(attack.targetIndex))
        infection.groups(attack.index).attack(immune, attack.targetIndex)
    }

    def fight(): (Int, Int) = {
      var (prevImmuneUnits, prevInfectionUnits) = (0, 0)
      while (immune.units > 0 && infection.units > 0) {
        if (prevImmuneUnits == immune.units && prevInfectionUnits == infection.units) return (0, 2)

        prevImmuneUnits = immune.units
        prevInfectionUnits = infection.units

        val attackQueue = mutable.Queue.from(
          (immune.selectTarget(infection) ++ infection.selectTarget(immune)).sortInPlaceBy(_.init).reverse)
        while (attackQueue.nonEmpty) nextAttack(attackQueue)
      }
      if (immune.units > 0) (immune.units, 0)
      else (infection.units, 1)
    }
  }

  def one: Int = {
    val result = Battle(immune.clone(), infection.clone()).fight()
    println(result)
    result._1
  }

  def two: Int = {
    var step = 10000
    var result = (0, 1)
    var boostValue = 0
    while (true) {
      while (result._2 != 0) {
        boostValue += step
        println(s"check $boostValue")
        val boostedImmune = immune.clone()
        boostedImmune.boost(boostValue)
        result = Battle(boostedImmune, infection.clone()).fight()
      }
      if (step == 1) {
        println(result)
        return boostValue
      }

      boostValue -= step
      step /= 10
      result = (0, 1)
    }
    0
  }
}