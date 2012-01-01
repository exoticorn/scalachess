package de.exoticorn.chess.base

class Position(val board: Board, val turn: Symbol, val castling: Seq[Square], val ep: Option[Square], val halfMoveClock: Int, val move: Int, val lastMove: Option[Move]) {
  val FEN = List(
    board.FEN,
    turn.name.take(1),
    XFENcastling,
    ep.map(s => s.toString).getOrElse("-"),
    halfMoveClock.toString,
    move.toString).mkString(" ")

  val positionKey = FEN.split(" ").take(4).mkString(" ")
  private val repeatCount: Map[String, Int] = lastMove match {
    case Some(m) => m.pos.repeatCount + (positionKey ->
      (1 + (if (m.pos.repeatCount.contains(positionKey)) m.pos.repeatCount(positionKey) else 0)))
    case None => Map(positionKey -> 1)
  }

  def XFENcastling: String = {
    val chars = for (sq <- castling) yield {
      val color = if (sq.rank == 1) 'white else 'black
      val kingSquare = board.find(Piece(color, 'king))
      val scanSquares = (sq to Square(if (sq -| kingSquare < 0) 'a' else 'h', sq.rank)).tail
      val char = if (scanSquares.forall(board(_) != Piece(color, 'rook)))
        if (sq -| kingSquare < 0) 'q' else 'k'
      else
        sq.file
      if (color == 'white) char.toUpper else char
    }

    if (chars.isEmpty)
      "-"
    else
      chars.mkString
  }

  def uciFEN: String = {
    val uciCastling = if (is960) {
      val chars = castling.map(sq => if (sq.rank == 1) sq.file.toUpper else sq.file)
      if (chars.isEmpty) "-" else chars.mkString
    } else XFENcastling
    "%s %s %s %s %d %d".format(
      board.FEN,
      turn.name.take(1),
      uciCastling,
      ep.map(s => s.toString).getOrElse("-"),
      halfMoveClock,
      move)
  }

  def notTurn: Symbol = if (turn == 'white) 'black else 'white

  def apply(move: Move): Position = {
    assert(move.pos == this)
    val newMoveNumber = this.move + (if (turn == 'white) 0 else 1)
    val notTurn = this.notTurn
    val newHalfMoveClock = board(move.to) match {
      case Piece(`notTurn`, _) => 0
      case _ => halfMoveClock + 1
    }
    val castling = board(move.to) match {
      case Piece(`notTurn`, 'rook) => for (sq <- this.castling if sq != move.to) yield sq
      case _ => this.castling
    }
    board(move.from) match {
      case Piece(_, 'king) =>
        val newCastling = for (sq <- castling if sq.rank != move.from.rank) yield sq
        board(move.to) match {
          case Piece(`turn`, 'rook) =>
            var newBoard = board.set(move.to, EmptySquare)
            val (kingTarget, rookTarget) = if (move.to -| move.from < 0) ('c', 'd') else ('g', 'f')
            newBoard = newBoard.move(move.from, Square(kingTarget, move.from.rank))
            newBoard = newBoard.set(Square(rookTarget, move.from.rank), Piece(turn, 'rook))
            new Position(newBoard, notTurn, newCastling, None, 0, newMoveNumber, Some(move))
          case _ =>
            val newBoard = board.move(move.from, move.to)
            new Position(newBoard, notTurn, newCastling, None, newHalfMoveClock, newMoveNumber, Some(move))
        }
      case Piece(_, 'pawn) =>
        var newBoard = board.move(move.from, move.to)
        if (move.from.file != move.to.file && board(move.to) == EmptySquare)
          newBoard = newBoard.set(Square(move.to.file, move.from.rank), EmptySquare)
        move.promoteTo.foreach(p => newBoard = newBoard.set(move.to, Piece(turn, p)))
        val newEP = if ((move.from -- move.to).abs == 2) Some(Square(move.from.file, (move.from.rank + move.to.rank) / 2))
        else None
        new Position(newBoard, notTurn, castling, newEP, 0, newMoveNumber, Some(move))
      case Piece(_, 'rook) =>
        val newBoard = board.move(move.from, move.to)
        val newCastling = for (sq <- castling if sq != move.from) yield sq
        new Position(newBoard, notTurn, newCastling, None, newHalfMoveClock, newMoveNumber, Some(move))
      case Piece(_, _) =>
        val newBoard = board.move(move.from, move.to)
        new Position(newBoard, notTurn, castling, None, newHalfMoveClock, newMoveNumber, Some(move))
    }
  }

  def isLegal: Boolean = {
    val kingSquare = board.find(Piece(notTurn, 'king))
    !board.isAttacked(kingSquare, turn)
  }

  def hasLegalMove: Boolean = {
    for (sq <- Square.all) board(sq) match {
      case Piece(`turn`, _) =>
        val moves = Move.generateMoves(this, sq)
        if (!moves.isEmpty) return true
      case _ =>
    }
    return false
  }

  def isDraw: Boolean = halfMoveClock >= 100 || repeatCount(positionKey) >= 3 || (!hasLegalMove && !isCheck)

  def isCheck: Boolean = board.isAttacked(board.find(Piece(turn, 'king)), notTurn)
  def isMate: Boolean = isCheck && !hasLegalMove
  def isFinished: Boolean = halfMoveClock >= 100 || repeatCount(positionKey) >= 3 || !hasLegalMove

  def moves: List[Move] = {
    def collectMoves(pos: Position, acc: List[Move]): List[Move] = pos.lastMove match {
      case Some(m) => collectMoves(m.pos, m :: acc)
      case None => acc
    }
    collectMoves(this, Nil)
  }

  def is960: Boolean = lastMove match {
    case Some(m) => m.pos.is960
    case None => !castling.forall { sq =>
      (sq.file == 'a' || sq.file == 'h') && board(Square('e', sq.rank)) == Piece(if (sq.rank == 1) 'white else 'black, 'king)
    }
  }

  override def equals(other: Any): Boolean = other match {
    case p: Position => p.FEN == FEN
    case _ => false
  }

  override def hashCode = FEN.hashCode
}

object Position {
  def apply(fen: String): Position = {
    fen.split(' ') match {
      case Array(pos, turnChar, castling, ep, clock, move) =>
        val board = Board.fromFEN(pos)

        val turn = if (turnChar == "w") 'white
        else if (turnChar == "b") 'black
        else throw new BadPositionException("Bad turn in FEN string '" + fen + "'")

        val castlingRooks = for (c <- castling if c != '-') yield {
          val color = if (c <= 'Z') 'white else 'black
          val rank = if (color == 'white) 1 else 8
          val file = c.toLower
          if (file <= 'h') {
            val square = Square(file, rank)
            if (board(square) == Piece(color, 'rook))
              square
            else
              throw new BadPositionException("No rook found for castling in '" + fen + "'")
          } else if (file == 'q' || file == 'k') {
            val searchSquares = if (file == 'q') (Square('a', rank) to Square('g', rank)) else (Square('h', rank) to Square('b', rank))
            searchSquares.find(board(_) == Piece(color, 'rook)) match {
              case Some(s) => s
              case _ => throw new BadPositionException("No rook found for castling in '" + fen + "'")
            }
          } else throw new BadPositionException("Bad character in FEN castling '" + fen + "'")
        }

        val epSquare = if (ep == "-") None else Some(Sq(ep))

        val halfMoveClock = clock.toInt
        val moveCount = move.toInt

        new Position(board, turn, sortedCastling(castlingRooks), epSquare, halfMoveClock, moveCount, None)
      case _ => throw new BadPositionException("Bad FEN string '" + fen + "'")
    }
  }

  def start: Position = apply("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  def from960(index: Int): Position = {
    val board = Board.from960(index)
    val castling = for (sq <- Square.all; Piece(_, 'rook) <- List(board(sq))) yield sq
    new Position(board, 'white, sortedCastling(castling), None, 0, 1, None)
  }

  def sortedCastling(squares: Seq[Square]): Seq[Square] = squares.sortBy(sq => sq.rank * 8 + ('h' - sq.file))
}

case class BadPositionException(e: String) extends Exception(e)
