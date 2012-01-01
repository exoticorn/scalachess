package de.exoticorn.chess.gui

import de.exoticorn.chess.base._
import scala.swing._
import event._

case class MoveMade(move: Move) extends event.Event

class SwingBoard(var interactive: Boolean = false) extends Component {
  private val pieceGfx = javax.imageio.ImageIO.read(new java.io.File("chess_set.png"))
  private val squareSize = 64

  private var position = Position.start
  private var dragStartSquare: Option[Square] = None
  private var dragPosition: java.awt.Point = new java.awt.Point(0, 0)

  minimumSize = new java.awt.Dimension(64 * 8, 64 * 8)
  preferredSize = minimumSize
  maximumSize = minimumSize

  private val pieceLookup = Map() ++ (for {
    (piece, x) <- Seq('king, 'queen, 'rook, 'bishop, 'knight, 'pawn).zipWithIndex
    (color, y) <- Seq('black, 'white).zipWithIndex
  } yield Piece(color, piece) -> (x * 64, y * 64))

  def setPosition(pos: Position) {
    position = pos
    repaint()
  }

  override def paintComponent(g: Graphics2D) {
    val dark = new java.awt.Color(96, 160, 160)
    val light = new java.awt.Color(160, 192, 192)

    for (x <- 0 to 7; y <- 0 to 7) {
      g.setColor(if (((x ^ y) & 1) == 0) light else dark)
      g.fillRect(x * squareSize, y * squareSize, squareSize, squareSize)
    }

    position.lastMove.foreach { m =>
      g.setColor(new java.awt.Color(0, 0, 0, 64))
      val (fx, fy) = squareToComponent(m.from)
      val (tx, ty) = squareToComponent(m.to)
      g.fillRect(fx, fy, squareSize, squareSize)
      g.fillRect(tx, ty, squareSize, squareSize)
    }

    for (sq <- Square.all; if Some(sq) != dragStartSquare; piece @ Piece(_, _) <- Some(position.board(sq))) {
      val (x, y) = squareToComponent(sq)
      drawPiece(g, piece, x, y)
    }

    for (sq <- dragStartSquare; piece @ Piece(_, _) <- Some(position.board(sq))) {
      drawPiece(g, piece, dragPosition.getX().toInt - squareSize / 2, dragPosition.getY().toInt - squareSize / 2)
    }
  }

  private def drawPiece(g: Graphics2D, piece: Piece, x: Int, y: Int) {
    val (sx, sy) = pieceLookup(piece)
    g.drawImage(pieceGfx, x, y, x + squareSize, y + squareSize, sx, sy, sx + squareSize, sy + squareSize, null)
  }

  private def posToSquare(pos: java.awt.Point) = {
    val x = (pos.getX() / squareSize).floor.toInt
    val y = 7 - (pos.getY() / squareSize).floor.toInt
    if (x < 0 || x > 7 || y < 0 || y > 7) None
    else Some(Square(('a' + x).toChar, y + 1))
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case MousePressed(_, pos, _, _, _) =>
      if (interactive) {
        val turn = position.turn
        for (sq <- posToSquare(pos); Piece(`turn`, _) <- Some(position.board(sq))) {
          dragStartSquare = Some(sq)
          dragPosition = pos
          repaint()
        }
      }
    case MouseDragged(_, pos, _) =>
      if (dragStartSquare != None) {
        dragPosition = pos
        repaint()
      }
    case MouseReleased(_, pos, _, _, _) =>
      for (from <- dragStartSquare; to <- posToSquare(pos)) {
        val moveString = from.toString + to.toString + (
          if (position.board(from) == Piece(position.turn, 'pawn) && (to.rank == 1 || to.rank == 8)) "q"
          else "")
        for (move <- Move.parse(position, moveString))
          publish(MoveMade(move))
      }
      dragStartSquare = None
      repaint()
  }

  private def squareToComponent(sq: Square) = ((sq.file - 'a').toInt * 64, (8 - sq.rank) * 64)
}

object SwingBoard extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "SwingBoard"
    contents = new SwingBoard
  }
}
