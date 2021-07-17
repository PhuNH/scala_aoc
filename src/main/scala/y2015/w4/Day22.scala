package y2015.w4

import common.Day
import common.Utils._

import scala.io.Source

class Day22 extends Day(inputPath(2015, 22)) {
  sealed abstract class Spell { val cost: Int; val dam: Int = 0; val timer: Int = 0 }
  object MagicMissile extends Spell { val cost = 53; override val dam = 4 }
  object Drain extends Spell { val cost = 73; override val dam = 2; val hp = 2 }
  object Shield extends Spell { val cost = 113; val ar = 7; override val timer = 6}
  object Poison extends Spell { val cost = 173; override val dam = 3; override val timer = 6 }
  object Recharge extends Spell { val cost = 229; val mana = 101; override val timer = 5 }

  case class Boss(hp: Int, dam: Int, effects: Map[Spell, Int] = Map.empty[Spell, Int]) {
    def this(t: IndexedSeq[Int]) = this(t(0), t(1))

    def takeSpell(s: Spell): Boss = s.timer match {
      case 0 => this.copy(hp = hp - s.dam)
      case t => this.copy(effects = effects.updated(s, t))
    }

    def takeEffect: Boss = effects.foldLeft(this)((b, e) => {
      val newHp = b.hp - e._1.dam
      val newEffects = if (e._2 <= 1) b.effects.removed(e._1) else b.effects.updated(e._1, e._2-1)
      b.copy(hp = newHp, effects = newEffects)
    })
  }

  case class Player(hp: Int, mana: Int,
                    spent: Int = 0, ar: Int = 0, effects: Map[Spell, Int] = Map.empty[Spell, Int]) {
    def takeDam(boss: Boss): Player = {
      val takenDam = if (ar >= boss.dam) 1 else boss.dam - ar
      this.copy(hp = hp - takenDam)
    }

    def cast(s: Spell, target: Boss): (Player, Boss) = {
      val newMana = mana - s.cost
      val newSpent = spent + s.cost
      s.timer match {
        case 0 =>
          val updatedBoss = target.takeSpell(s)
          if (s == Drain) (this.copy(hp = hp + Drain.hp, mana = newMana, spent = newSpent), updatedBoss)
          else (this.copy(mana = newMana, spent = newSpent), updatedBoss)
        case t =>
          if (s == Poison) (this.copy(mana = newMana, spent = newSpent), target.takeSpell(s))
          else (this.copy(mana = newMana, spent = newSpent, effects = effects.updated(s, t)), target)
      }
    }

    def takeEffect: Player = effects.foldLeft(this.copy(ar = 0))((p, e) => {
      val newAr = if (e._1 == Shield && e._2 > 1) p.ar + Shield.ar else p.ar
      val newMana = if (e._1 == Recharge) p.mana + Recharge.mana else p.mana
      val newEffects = if (e._2 <= 1) p.effects.removed(e._1) else p.effects.updated(e._1, e._2-1)
      p.copy(mana = newMana, ar = newAr, effects = newEffects)
    })
  }

  private val data: IndexedSeq[Int] = using(Source.fromResource(inputs(0)))(
    _.getLines().toIndexedSeq.map(_.split(':')(1).tail.toInt))
  private val boss: Boss = new Boss(data)
  private val player: Player = Player(50, 500)
  private val spells = IndexedSeq(MagicMissile, Drain, Shield, Poison, Recharge)

  private def fight(player: Player, boss: Boss, turn: Int, isHard: Boolean = false): (Int, Int) = {
    if (turn % 2 == 0) {
      if (isHard && player.hp <= 1) (0, player.spent)
      else {
        val playerWithMode = if (isHard) player.copy(hp = player.hp - 1) else player
        val updatedPlayer = playerWithMode.takeEffect
        val updatedBoss = boss.takeEffect
        if (updatedBoss.hp <= 0) (1, updatedPlayer.spent)
        else {
          val allCases = for (s <- spells if !updatedBoss.effects.contains(s) && !updatedPlayer.effects.contains(s)) yield {
            val (nextUpdatedPlayer, nextUpdatedBoss) = updatedPlayer.cast(s, updatedBoss)
            if (nextUpdatedPlayer.mana <= 0) (0, nextUpdatedPlayer.spent)
            else if (nextUpdatedBoss.hp <= 0) (1, nextUpdatedPlayer.spent)
            else fight(nextUpdatedPlayer, nextUpdatedBoss, turn+1, isHard)
          }
          val winningCases = allCases.filter(_._1 == 1)
          if (winningCases.nonEmpty) winningCases.minBy(_._2)
          else (0, 0)
        }
      }
    } else {
      val updatedPlayer = player.takeEffect
      val updatedBoss = boss.takeEffect
      if (updatedBoss.hp <= 0) (1, updatedPlayer.spent)
      else {
        val nextUpdatedPlayer = updatedPlayer.takeDam(updatedBoss)
        if (nextUpdatedPlayer.hp <= 0) (0, nextUpdatedPlayer.spent)
        else fight(nextUpdatedPlayer, updatedBoss, turn+1, isHard)
      }
    }
  }

  def one: Int = fight(player, boss, 0)._2

  def two: Int = fight(player, boss, 0, isHard = true)._2
}