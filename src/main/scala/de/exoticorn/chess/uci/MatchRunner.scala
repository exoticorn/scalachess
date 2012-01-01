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

    override def toString = "+%d -%d =%d".format(winA, winB, draws)
  }

  val board = new SwingBoard
  def top = new MainFrame {
    title = "Engine Match"
    contents = board
  }

  actor {
    val timeBase = 60
    val timeInc = 1
    val time = Time(timeBase, timeInc, timeBase, timeInc)
    val engineCmds = ("stockfish-2.1.1", "stockfish")

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
