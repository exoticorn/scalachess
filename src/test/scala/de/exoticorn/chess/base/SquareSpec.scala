import org.scalatest.FlatSpec
import de.exoticorn.chess.base._

class SquareSpec extends FlatSpec {
	
	"The Square e2" should "return correct file and rank" in {
		assert(Sq('e2).file === 'e')
		assert(Sq('e2).rank === 2)
	}
	
	"The squares e2 and h4" should "be 3 files apart" in { assert(Sq('h4) -| Sq('e2) === 3) }
	it should "be 2 ranks apart" in { assert(Sq('h4) -- Sq('e2) === 2) }

  "Square.to" should "work horizontally" in {
    assert((Sq('e3) to Sq('g3)).toArray === Array(Sq('e3), Sq('f3), Sq('g3)))
    assert((Sq('f6) to Sq('a6)).toArray === Array(Sq('f6), Sq('e6), Sq('d6), Sq('c6), Sq('b6), Sq('a6)))
  }
}
