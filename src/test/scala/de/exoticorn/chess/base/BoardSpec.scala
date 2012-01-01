import org.scalatest.FlatSpec
import de.exoticorn.chess.base._

class BoardSpec extends FlatSpec {
	"An empty board" should "be filled with EmptySquare" in {
		val board = Board.empty
		assert(Square.all.forall (board(_) == EmptySquare))
	}

  val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  val startPosition = Position(startFEN)
	
	"The starting board" should "have all pieces at their right position" in {
		val board = startPosition.board
		def assertColor(squares: Seq[Square], color: Symbol) =
			for(sq <- squares) assert(board(sq) match { case Piece(`color`, _) => true; case _ => false})
		def assertPiece(squares: Seq[Square], piece: Symbol) =
			for(sq <- squares) assert(board(sq) match { case Piece(_, `piece`) => true; case _ => false})
		assertColor(Sq('a1) rectTo Sq('h2), 'white)
		assertColor(Sq('a8) rectTo Sq('h7), 'black)
		assertPiece((Sq('a2) rectTo Sq('h2)) ++ (Sq('a7) rectTo Sq('h7)), 'pawn)
		assertPiece(List(Sq('a1), Sq('h1), Sq('a8), Sq('h8)), 'rook)
		assertPiece(List(Sq('b1), Sq('g1), Sq('b8), Sq('g8)), 'knight)
		assertPiece(List(Sq('c1), Sq('f1), Sq('c8), Sq('f8)), 'bishop)
		assertPiece(List(Sq('d1), Sq('d8)), 'queen)
		assertPiece(List(Sq('e1), Sq('e8)), 'king)
	}

  it should  "respond to toFEN with the right FEN string" in { assert(startPosition.FEN === startFEN)}
}

