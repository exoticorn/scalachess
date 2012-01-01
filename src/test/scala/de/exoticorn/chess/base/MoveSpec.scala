import org.scalatest.FlatSpec
import de.exoticorn.chess.base._

class MoveSpec extends FlatSpec {
  val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  "A simple test game" should "produce the correct FEN" in {
    var position = Position(startFEN)
    position = position(Move(position, "e4"))
    assert(position.FEN === "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
    position = position(Move(position, "c5"))
    assert(position.FEN === "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")
    position = position(Move(position, "Nf3"))
    assert(position.FEN === "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
  }

  "A more complete game" should "produce the correct FEN" in {
    var position = Position(startFEN)
    for(move <- "e4 e5 Nf3 a5 Nc3 Ra6 Bxa6 bxa6 O-O Nc6 d4 Nge7 Bg5 Nb4 a3 Nbd5".split(" "))
      position = position(Move(position, move))
    assert(position.FEN === "2bqkb1r/2ppnppp/p7/p2np1B1/3PP3/P1N2N2/1PP2PPP/R2Q1RK1 w k - 1 9")
  }

  "A game with ep and promotions" should "produce the correct FEN" in {
    var position = Position(startFEN)
    for(move <- "Nf3 d5 Rg1 d4 c4 dxc3 d4 cxb2 d5 bxa1=N d6 b5 dxc7 b4 cxd8=B b3 e4 bxa2 e5 f5 exf6 axb1=R fxg7 Ba6 gxh8=Q Rb5 Bc7 Nf6 Qd8+ Kf7 Bc4+ Kg6 Nh4+ Kh5 Qd1+ Kxh4 B7f4 Nbd7 Qf3 Rc5 Qg3+ Kh5 Be2+ Ng4 Qxg4#".split(" ")) {
      val m = Move(position, move)
      assert(m.toString === move)
      position = position(m)
    }
    assert(position.FEN === "r4b1Q/p2np2p/b7/2r4k/5BQ1/8/4BPPP/n1B1K1R1 b - - 0 23")
  }

  "A pinned piece" should "not be able to move" in {
    var position = Position(startFEN)
    for(move <- "d4 e6 Nd2 Bb4 Nh3 Nf6 f3 Nc6 Nf2 d6 Ne4".split(" "))
      position = position(Move(position, move))
    assert(position.FEN === "r1bqk2r/ppp2ppp/2nppn2/8/1b1PN3/5P2/PPPNP1PP/R1BQKB1R b KQkq - 1 6")
  }

  "A threefold repetition" should "result in a draw" in {
    var position = Position(startFEN)
    for(move <- "Nf3 Nc6 Ng1 Nb8 Nf3 Nc6 Ng1 Nb4 Nf3 Nc6".split(" ")) {
      assert(!position.isDraw)
      position = Move(position, move).result
    }
    assert(position.isDraw)
  }

  "The lib" should "not be buggy" in {
    assert(!Position("6k1/7p/4P1p1/8/5q2/7P/6RK/1q6 w - - 2 56").isMate)
  }
}

