package de.exoticorn.chess.uci

import de.exoticorn.chess.base._

class Clock(var time: Time) {
  var turn = 'white
  var startTime: Option[Long] = None

  def start(t: Symbol) {
    turn = t
    startTime = Some(System.currentTimeMillis())
  }

  def stop() {
    val timeSpent = (System.currentTimeMillis() - startTime.get) / 1000.0
    if (turn == 'white) {
      val timeLeft = (time.wtime - timeSpent + time.winc).max(0)
      time = Time(timeLeft, time.winc, time.btime, time.binc)
    } else {
      val timeLeft = (time.btime - timeSpent + time.binc).max(0)
      time = Time(time.wtime, time.winc, timeLeft, time.binc)
    }
    startTime = None
  }
}

case class NewPositionMsg(pos: Position)
case class GameFinished(result: Double)
case object ScoreBook extends Score {
  override def toString = "book"
}

class EngineGame(channel: actors.OutputChannel[Any], engineCmdA: String, engineCmdB: String, time: Time,
  var position: Position = Position.start,
  val book: Option[PolyglotBook] = None) extends actors.DaemonActor {

  val engineA = new EngineCore(engineCmdA, "1")
  val engineB = new EngineCore(engineCmdB, "2")

  val clock = new Clock(time)

  var latestInfo = Info("", 0, ScoreBook)

  def act() {
    printf("Starting game: %s vs. %s\n", engineCmdA, engineCmdB)

    channel ! NewPositionMsg(position)

    startThink()

    while (!position.isFinished) {
      receive {
        case BestMove(id, move) =>
          clock.stop()
          printf("%3d.%s%-6s %6s (%2d)   [wtime: %.2f, btime: %.2f]\n",
            position.move, (if (position.turn == 'white) "  " else ".."), move.toString,
            latestInfo.score.toString, latestInfo.depth,
            clock.time.wtime, clock.time.btime)
          position = move.result
          channel ! NewPositionMsg(position)
          startThink()
        case info: Info => latestInfo = info
      }
    }

    val result =
      if (position.isDraw) 0.5
      else if (position.turn == 'black) 1
      else 0

    engineA ! Quit
    engineB ! Quit

    channel ! GameFinished(result)
  }

  def startThink() {
    if (position.isFinished) return
    val engine = if (position.turn == 'white) engineA else engineB
    latestInfo = Info("", 0, ScoreBook)
    clock.start(position.turn)
    book.flatMap(_.randomMove(position)) match {
      case Some(entry) => this ! BestMove(engine.id, entry.move)
      case None =>
        engine ! FindBestMove(this, position, clock.time)
    }
  }
}
