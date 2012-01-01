package de.exoticorn.chess.base

import java.lang.Boolean

class Move private (val pos: Position, val from: Square, val to: Square, val promoteTo: Option[Symbol]) {
  private def isLegal: Boolean = {
    if (to == from) return false
    val notTurn = pos.notTurn
    if (pos.board(to) == Piece(notTurn, 'king)) return false
    val toNotOwn = pos.board(to) match { case Piece(pos.turn, _) => false; case _ => true }
    val isMovePossible: Boolean = pos.board(from) match {
      case Piece(pos.turn, 'pawn) =>
        if ((to.rank == 1 || to.rank == 8) && promoteTo == None) false
        else if ((to.rank != 1 && to.rank != 8) && promoteTo != None) false
        else {
          val dir = if (pos.turn == 'white) 1 else -1
          if (to -- from == dir * 2)
            from.file == to.file && pos.board(Square(to.file, (from.rank + to.rank) / 2)) == EmptySquare &&
              pos.board(to) == EmptySquare
          else if (to -- from == dir) {
            if (from.file == to.file) pos.board(to) == EmptySquare
            else if ((to -| from).abs == 1) {
              pos.ep == Some(to) || (pos.board(to) match { case Piece(`notTurn`, _) => true; case _ => false })
            } else false
          } else false
        }
      case Piece(pos.turn, 'knight) =>
        val (dx, dy) = ((to -| from).abs, (to -- from).abs)
        ((dx == 1 && dy == 2) || (dx == 2 && dy == 1)) && toNotOwn
      case Piece(pos.turn, 'bishop) => canBishopMove && toNotOwn
      case Piece(pos.turn, 'rook) => canRookMove && toNotOwn
      case Piece(pos.turn, 'queen) => (canRookMove || canBishopMove) && toNotOwn
      case Piece(pos.turn, 'king) =>
        if (pos.board(to) == Piece(pos.turn, 'rook)) canCastle
        else canKingMove && toNotOwn
      case _ => false
    }
    isMovePossible && pos(this).isLegal
  }

  private def canBishopMove: Boolean = (to -| from).abs == (to -- from).abs && canRangedMove
  private def canRookMove: Boolean = (to.file == from.file || to.rank == from.rank) && canRangedMove
  private def canRangedMove: Boolean = (to between from).forall(sq => pos.board(sq) == EmptySquare)

  private def canKingMove = (to -| from).abs <= 1 && (to -- from).abs <= 1

  private def canCastle: Boolean = {
    val rank = if (pos.turn == 'white) 1 else 8
    if (from.rank != rank || to.rank != rank) return false
    if (!pos.castling.contains(to)) return false
    val (kt, rt) = if (to -| from < 0) ('c', 'd') else ('g', 'f')
    val files = List(from.file, to.file, kt, rt)
    var squaresFree = (Square(files.min, rank) to Square(files.max, rank)).forall { sq =>
      sq == from || sq == to || pos.board(sq) == EmptySquare
    }
    if (!squaresFree) return false
    (from to Square(kt, rank)).init.forall(sq => !pos.board.isAttacked(sq, pos.notTurn))
  }

  def result = pos(this)

  override def toString = {
    val move = pos.board(from) match {
      case Piece(pos.turn, 'king) =>
        pos.board(to) match {
          case Piece(pos.turn, 'rook) => if (to -| from < 0) "O-O-O" else "O-O"
          case EmptySquare => "K" + to
          case _ => "Kx" + to
        }
      case Piece(pos.turn, 'pawn) =>
        val m = if (to.file == from.file) to.toString
        else from.file.toString + "x" + to.toString
        promoteTo.map(p => m + "=" + Move.pieceToChar(p).toString).getOrElse(m)
      case Piece(pos.turn, piece) =>
        val conflictingSquares = for {
          sq <- pos.board.findAll(Piece(pos.turn, piece))
          if sq != from
          conflictingMove = new Move(pos, sq, to, None)
          if conflictingMove.isLegal
        } yield sq
        var m = Move.pieceToChar(piece).toString
        if (!conflictingSquares.isEmpty) {
          val sameFile = conflictingSquares.exists(_.file == from.file)
          val sameRank = conflictingSquares.exists(_.rank == from.rank)
          if (sameFile && sameRank) m += from
          else if (sameFile) m += from.rank.toString
          else m += from.file.toString
        }
        val notTurn = pos.notTurn
        pos.board(to) match { case Piece(`notTurn`, _) => m += "x"; case _ => }
        m + to
    }
    val targetPosition = pos(this)
    if (targetPosition.isCheck) move + (if (targetPosition.hasLegalMove) "+" else "#")
    else move
  }

  def uci: String = {
    if (pos.board(from) == Piece(pos.turn, 'king) && pos.board(to) == Piece(pos.turn, 'rook) && !pos.is960) {
      from.toString + (if (to -| from < 0) "c" + to.rank.toString
      else "g" + to.rank.toString)
    } else from.toString + to.toString + promoteTo.map(p => Move.pieceToChar(p).toLower.toString).getOrElse("")
  }
}

object Move {
  private val CastlingPattern = """[oO0]-[oO0](-[oO0])?[+#]?""".r
  private val RegularMovePattern = """([NBRKQ])?([a-h])?([1-8])?x?([a-h][1-8])=?([NBRQnbrq])?[+#]?""".r

  private val charToPiece = Map('N' -> 'knight, 'B' -> 'bishop, 'R' -> 'rook, 'Q' -> 'queen, 'K' -> 'king)
  private val pieceToChar = Map('knight -> 'N', 'bishop -> 'B', 'rook -> 'R', 'queen -> 'Q', 'king -> 'K')

  def apply(pos: Position, move: String): Move = {
    val candidates = move match {
      case CastlingPattern(long) => Seq(createCastlingMove(pos, long != null))
      case RegularMovePattern(piece, fromFile, fromRank, toString, promoteToPiece) =>
        val to = Sq(toString)
        val promoteTo = if (promoteToPiece == null) None else Some(charToPiece(promoteToPiece(0).toUpper))
        val candidates = if (fromFile != null && fromRank != null) {
          val from = Sq(fromFile + fromRank)
          pos.board(from) match {
            case Piece(pos.turn, 'king) =>
              if (pos.board(to) == Piece(pos.turn, 'rook) || (to -| from).abs > 1)
                Seq(createCastlingMove(pos, to -| from < 0))
              else
                Seq(new Move(pos, from, to, None))
            case Piece(pos.turn, piece) => Seq(new Move(pos, from, to, promoteTo))
          }
        } else if (piece == null) {
          val dir = if (pos.turn == 'white) 1 else -1
          val dirs = Seq((0, -dir), (0, -dir * 2), (-1, -dir), (1, -dir))
          for (d <- dirs; sq <- to +? d if pos.board(sq) == Piece(pos.turn, 'pawn)) yield new Move(pos, sq, to, promoteTo)
        } else for (from <- pos.board.findAll(Piece(pos.turn, charToPiece(piece(0)))))
          yield new Move(pos, from, to, None)
        for {
          m <- candidates
          Piece(_, p) <- Some(pos.board(m.from))
          if (piece == null || p == charToPiece(piece(0)))
          if (fromFile == null || m.from.file == fromFile(0))
          if (fromRank == null || m.from.rank == fromRank.toInt)
        } yield m
    }
    val legalCandidates = for (m <- candidates if m.isLegal) yield m
    if (legalCandidates.isEmpty) throw new BadMoveException("No legal move found for pattern '" + move + "'")
    if (legalCandidates.length > 1) throw new BadMoveException("Ambigious move '" + move + "'")
    legalCandidates.head
  }

  def parse(pos: Position, move: String): Option[Move] = try {
    Some(this(pos, move))
  } catch {
    case _: BadMoveException => None
  }

  private def createCastlingMove(pos: Position, long: Boolean) = {
    val rank = if (pos.turn == 'white) 1 else 8
    val kingSquare = pos.board.find(Piece(pos.turn, 'king))
    val rookSquares = for (sq <- pos.castling if sq.rank == rank && (sq -| kingSquare < 0) == long) yield sq
    if (rookSquares.isEmpty) throw new BadMoveException("Found no rook to castle with")
    val rookSquare = rookSquares.head
    new Move(pos, kingSquare, rookSquare, None)
  }

  private val knightDirs = Seq((1, 2), (2, 1), (-1, 2), (-2, 1), (1, -2), (2, -1), (-1, -2), (-2, -1))

  def generateMoves(pos: Position, sq: Square): Seq[Move] = {
    val candidates = pos.board(sq) match {
      case Piece(pos.turn, 'king) =>
        val kingMoves = for (x <- -1 to 1; y <- -1 to 1; if x != 0 || y != 0; to <- sq +? (x, y))
          yield new Move(pos, sq, to, None)
        val castlingMoves = for (to <- pos.castling if to.rank == sq.rank) yield new Move(pos, sq, to, None)
        kingMoves ++ castlingMoves
      case Piece(pos.turn, 'pawn) =>
        val dir = if (pos.turn == 'white) 1 else -1
        val dirs = Seq((0, dir), (0, dir * 2), (-1, dir), (1, dir))
        for (d <- dirs; to <- sq +? d) yield {
          if (to.rank == 1 || to.rank == 8) new Move(pos, sq, to, Some('queen))
          else new Move(pos, sq, to, None)
        }
      case Piece(pos.turn, 'bishop) => generateBishopMoves(pos, sq)
      case Piece(pos.turn, 'rook) => generateRookMoves(pos, sq)
      case Piece(pos.turn, 'queen) => generateBishopMoves(pos, sq) ++ generateRookMoves(pos, sq)
      case Piece(pos.turn, 'knight) =>
        for (d <- knightDirs; to <- sq +? d) yield new Move(pos, sq, to, None)
    }
    for (move <- candidates if move.isLegal) yield move
  }

  private def generateBishopMoves(pos: Position, from: Square) =
    generateRangeMoves(pos, from, Seq((1, 1), (-1, 1), (1, -1), (-1, -1)))

  private def generateRookMoves(pos: Position, from: Square) =
    generateRangeMoves(pos, from, Seq((1, 0), (-1, 0), (0, -1), (0, 1)))

  private def generateRangeMoves(pos: Position, from: Square, dirs: Seq[(Int, Int)]) = {
    def generateRangeMoves(sq: Square, dir: (Int, Int), acc: List[Move]): List[Move] = (sq +? dir) match {
      case Some(newSq) => pos.board(newSq) match {
        case Piece(pos.turn, _) => acc
        case EmptySquare => generateRangeMoves(newSq, dir, (new Move(pos, from, newSq, None)) :: acc)
        case _: Piece => (new Move(pos, from, newSq, None)) :: acc
      }
      case None => acc
    }
    for (dir <- dirs; move <- generateRangeMoves(from, dir, Nil)) yield move
  }
}

case class BadMoveException(e: String) extends Exception(e)