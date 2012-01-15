package de.exoticorn.chess.uci

import de.exoticorn.chess.base._
import de.exoticorn.chess.gui._
import swing._
import scala.actors.Actor._

object MatchRunner extends SimpleSwingApplication {
  class Results {
    var winA = 0
    var winB = 0
    var draws = 0

    def +=(result: Double) {
      if (result == 0) winB += 1
      else if (result == 1) winA += 1
      else draws += 1
    }
    
    def eloDifference: Option[Int] = {
      if(draws == 0 && (winA == 0 || winB == 0)) None
      else {
        val total = winA + winB + draws
        val score = winA + draws * 0.5
        val percentage = score / total
        val elo = -400 * math.log10(1 / percentage - 1)
        Some(elo.round.toInt)
      }
    }
    
    def eloDifferenceString = eloDifference.map(d => " (%s%d)".format((if(d > 0) "+" else ""), d)).getOrElse("")

    override def toString = "+%d -%d =%d%s".format(winA, winB, draws, eloDifferenceString)
  }

  val board = new SwingBoard
  def top = new MainFrame {
    title = "Engine Match"
    contents = board
  }

  actor {
    val timeBase = 60*5
    val timeInc = 2
    val time = Time(timeBase, timeInc, timeBase, timeInc)
    val engineCmds = ("stockfish", "toga2")

    val chess960 = false
    val rng = new scala.util.Random

    val book = PolyglotBook("book.bin")

    var evenGame = true
    val results = new Results

    def startNextGame() {
      System.gc()
      val engines = if (evenGame) engineCmds else (engineCmds._2, engineCmds._1)
      val position =
        if (chess960) Position.from960(rng.nextInt(960))
        else Position.start
      val game = new EngineGame(self, engines._1, engines._2, time, position, book = Some(book))
      game.start()
    }

    startNextGame()

    while (true) {
      receive {
        case NewPositionMsg(pos) => Swing.onEDT(board.setPosition(pos))
        case GameFinished(result) =>
          results += (if (evenGame) result else 1 - result)
          printf("%s: %s vs. %s\n", results, engineCmds._1, engineCmds._2)
          evenGame = !evenGame
          startNextGame()
      }
    }
  }
}
