package de.exoticorn.chess.base

class Board private (data: Array[SquareContent]) {
  assert(data.length == 64)

  def apply(sq: Square): SquareContent = data(sq.x + sq.y * 8)

  def FEN: String = {
    val pieces = Map('pawn -> 'p', 'rook -> 'r', 'knight -> 'n', 'bishop -> 'b', 'queen -> 'q', 'king -> 'k')
    (for (y <- 7 to 0 by -1) yield {
      val rank = for (x <- 0 to 7) yield data(x + y * 8) match {
        case EmptySquare => '#'
        case Piece('black, piece) => pieces(piece)
        case Piece('white, piece) => pieces(piece).toUpper
      }
      "#+".r.replaceAllIn(rank.mkString, _.matched.length.toString)
    }).mkString("/")
  }

  def findAll(piece: Piece): Seq[Square] = for (sq <- Square.all if this(sq) == piece) yield sq
  def find(piece: Piece): Square = findAll(piece).head

  def isAttacked(sq: Square, color: Symbol): Boolean = {
    val pawnDirs = if (color == 'white) Seq((-1, -1), (1, -1)) else Seq((-1, 1), (1, 1))
    for (dir <- pawnDirs; sq <- sq +? dir if this(sq) == Piece(color, 'pawn)) return true
    for (dir <- Board.knightDirs; sq <- sq +? dir if this(sq) == Piece(color, 'knight)) return true
    if (Board.rookDirs.exists(dir => checkRangeAttack(sq, dir, p => p == Piece(color, 'rook) || p == Piece(color, 'queen))))
      return true
    if (Board.bishopDirs.exists(dir => checkRangeAttack(sq, dir, p => p == Piece(color, 'bishop) || p == Piece(color, 'queen))))
      return true
    for (dir <- Board.kingDirs; sq <- sq +? dir if this(sq) == Piece(color, 'king)) return true
    false
  }

  private def checkRangeAttack(sq: Square, dir: (Int, Int), p: Piece => Boolean): Boolean = {
    (sq +? dir).map { nextSq =>
      this(nextSq) match {
        case EmptySquare => checkRangeAttack(nextSq, dir, p)
        case piece: Piece => p(piece)
      }
    }.getOrElse(false)
  }

  def set(sq: Square, v: SquareContent): Board = {
    val newData = data.clone()
    newData(sq.x + sq.y * 8) = v
    new Board(newData)
  }

  def move(from: Square, to: Square): Board = {
    val newData = data.clone()
    val piece = newData(from.x + from.y * 8)
    newData(from.x + from.y * 8) = EmptySquare
    newData(to.x + to.y * 8) = piece
    new Board(newData)
  }
}

object Board {
  def empty: Board = new Board(Array.fill[SquareContent](64)(EmptySquare))

  def fromFEN(fenPos: String): Board = {
    val pieces = Map('p' -> 'pawn, 'r' -> 'rook, 'n' -> 'knight, 'b' -> 'bishop, 'q' -> 'queen, 'k' -> 'king)
    val ranks = for (rank <- fenPos.split('/')) yield {
      val expandedRank = "\\d".r.replaceAllIn(rank, "#" * _.matched.toInt)
      val squares = for (char <- expandedRank) yield char match {
        case '#' => EmptySquare
        case _ =>
          val color = if (char <= 'Z') 'white else 'black
          Piece(color, pieces(char.toLower))
      }
      if (squares.length != 8) throw new BadPositionException("Bad position inFEN string: '" + fenPos + "'")
      squares.toArray
    }
    if (ranks.length != 8) throw new BadPositionException("Bad position inFEN string: '" + fenPos + "'")
    new Board(ranks.reverse.toArray.flatten)
  }

  def from960(i: Int): Board = {
    var index = i
    assert(index >= 0 && index < 960)
    val data = Array.fill[SquareContent](64)(EmptySquare)
    def set(x: Int, p: Symbol) {
      data(x) = Piece('white, p)
      data(x + 7 * 8) = Piece('black, p)
    }
    set((index % 4) * 2 + 1, 'bishop)
    index /= 4
    set((index % 4) * 2, 'bishop)
    index /= 4
    def nth(n: Int, x: Int = 0): Int =
      if (data(x) == EmptySquare) {
        if (n == 0) x
        else nth(n - 1, x + 1)
      } else nth(n, x + 1)
    set(nth(index % 6), 'queen)
    val (x1, x2) = Seq((0, 1), (0, 2), (0, 3), (0, 4), (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4))((index / 6) % 10)
    set(nth(x2), 'knight)
    set(nth(x1), 'knight)
    set(nth(0), 'rook)
    set(nth(0), 'king)
    set(nth(0), 'rook)
    for (x <- 0 to 7) {
      data(x + 1 * 8) = Piece('white, 'pawn)
      data(x + 6 * 8) = Piece('black, 'pawn)
    }
    new Board(data)
  }

  private val knightDirs = Seq((1, 2), (2, 1), (-1, 2), (-2, 1), (1, -2), (2, -1), (-1, -2), (-2, -1))
  private val rookDirs = Seq((1, 0), (0, 1), (-1, 0), (0, -1))
  private val bishopDirs = Seq((1, 1), (-1, 1), (1, -1), (-1, -1))
  private val kingDirs = rookDirs ++ bishopDirs
}

