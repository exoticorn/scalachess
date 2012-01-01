package de.exoticorn.chess.uci

import de.exoticorn.chess.base._
import de.exoticorn.chess.gui._
import swing._
import scala.actors.Actor._

object HumanGameGUI extends SimpleSwingApplication {
  val hbox = new BoxPanel(Orientation.Horizontal)
  val board = new SwingBoard(interactive = true)
  val moveArea = new TextArea
  moveArea.minimumSize = new java.awt.Dimension(256, 512)
  moveArea.preferredSize = new java.awt.Dimension(256, 512)
  moveArea.editable = false
  moveArea.lineWrap = true
  moveArea.wordWrap = true
  hbox.contents += board
  hbox.contents += moveArea
  var position = Position.start
  board.setPosition(position)
  def top = new MainFrame {
    title = "Play against the computer!"
    contents = hbox
  }

  listenTo(board)

  reactions += {
    case MoveMade(move) =>
      setPosition(move.result)
      board.interactive = false
      if (!position.isFinished) {
        engineActor ! Go(position)
      }
  }

  case class Go(pos: Position)

  val engineActor = actor {
    val engine = new EngineCore("stockfish")
    engine ! SetOption("Skill Level", "1")
    val time = Time(60, 0, 60, 0)
    while (true) {
      receive {
        case Go(pos) => engine ! FindBestMove(self, position, time)
        case BestMove(_, move) =>
          Swing.onEDT {
            setPosition(move.result)
            board.interactive = !position.isFinished
          }
      }
    }
  }

  private def setPosition(pos: Position) {
    position = pos
    board.setPosition(pos)
    moveArea.text = PositionTree(pos).toString
  }
}