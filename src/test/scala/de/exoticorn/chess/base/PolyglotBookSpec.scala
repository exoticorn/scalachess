import org.scalatest.FlatSpec
import de.exoticorn.chess.base._

class PolyglotBookSpec extends FlatSpec {
	"The polyglot book hash function" should "evaluate the test positions correctly" in {
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")) === 0x463b96181691fc9cL)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")) === 0x823c9b50fd114196L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2")) === 0x0756b94461c50fb0L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")) === 0x662fafb965db29d4L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")) === 0x22a48b5a8e47ff78L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3")) === 0x652a607ca3f242c1L)
		assert(PolyglotBook.calcPositionHash(Position("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 0 4")) === 0x00fdd303c946bdd9L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3")) === 0x3c8123ea7b067637L)
		assert(PolyglotBook.calcPositionHash(Position("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 0 4")) === 0x5c3f9b829b279560L)
	}
  
	"The starting position" should "have multiple entries in the book" in {
		val book = PolyglotBook("book.bin")
		assert(book(Position.start).length > 0)
	}
}