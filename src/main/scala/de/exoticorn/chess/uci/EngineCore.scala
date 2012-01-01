package de.exoticorn.chess.uci
import de.exoticorn.chess.base._

case class Time(wtime: Double, winc: Double, btime: Double, binc: Double)

class Score
case class ScorePawns(p: Double) extends Score {
  override def toString = "%.2f".format(p)
}
case class ScoreMate(md: Int) extends Score {
  override def toString = "#%d".format(md)
}

sealed class EngineCommand
case class FindBestMove(channel: actors.OutputChannel[Any], pos: Position, time: Time) extends EngineCommand
case object Quit extends EngineCommand
case class SetOption(name: String, value: String) extends EngineCommand

sealed class EngineOutput
case object Started extends EngineOutput
case class BestMove(id: String, m: Move) extends EngineOutput
case class Info(id: String, depth: Int, score: Score) extends EngineOutput

class EngineCore(command: String, val id: String = "UCIengine") extends actors.DaemonActor {
  val builder = sys.process.Process(command)
  var os: java.io.OutputStream = _
  val io = new sys.process.ProcessIO(
    (in => {
      in.write("uci\n".getBytes())
      in.flush()
      os = in
    }),
    (out => {
      var line = ""
      var running = true
      while (running) {
        try {
          val byte = out.read()
          if (byte < 32) {
            if (!line.isEmpty()) {
              //							println("read line: " + line)
              parseLine(line)
              line = ""
            }
          } else {
            line += byte.toChar.toString
          }
        } catch {
          case e: java.io.IOException => running = false
        }
      }
    }),
    (err => {}))

  val process = builder.run(io)

  var position = Position.start
  var channel: Option[actors.OutputChannel[Any]] = None

  var readySyncVar = new concurrent.SyncVar[Unit]
  readySyncVar.get

  start()

  def act() {
    var running = true
    while (running) {
      receive {
        case FindBestMove(ch, pos, time) =>
          channel = Some(ch)
          position = pos
          val moves = pos.moves
          command("setoption name UCI_Chess960 value " + (if (pos.is960) "true" else "false"))
          if (moves.isEmpty) command("position fen " + pos.uciFEN)
          else command("position fen " + moves.head.pos.uciFEN + " moves " + moves.map(m => m.uci).mkString(" "))
          command("go wtime %d btime %d winc %d binc %d".format(
            (time.wtime * 1000).toInt, (time.btime * 1000).toInt, (time.winc * 1000).toInt, (time.binc * 1000).toInt))
        case Quit => running = false
        case SetOption(name, value) => command("setoption name %s value %s".format(name, value))
        case m => println("Unknown message: " + m.toString)
      }
    }
    process.destroy()
  }

  def command(cmd: String) {
    //		println(cmd)
    os.write((cmd + "\n").getBytes())
    os.flush()
  }

  def parseLine(line: String) {
    var parts = line.split(" ").toList
    if (parts.isEmpty) return
    parts.head match {
      case "uciok" => readySyncVar.set(())
      case "bestmove" =>
        channel.foreach(_ ! BestMove(id, Move(position, parts.tail.head)))
        channel = None
      case "info" =>
        var depth: Option[Int] = None
        var score: Option[Score] = None
        parts = parts.tail
        while (!parts.isEmpty) {
          var head = parts.head
          parts = parts.tail
          head match {
            case "depth" =>
              depth = Some(parts.head.toInt)
              parts = parts.tail
            case "seldepth" => parts = parts.tail
            case "multipv" => parts = parts.tail
            case "score" =>
              val t = parts.head
              val value = parts.tail.head.toInt * (if (position.turn == 'white) 1 else -1)
              parts = parts.tail.tail
              score = if (t == "cp") Some(ScorePawns(value / 100.0)) else Some(ScoreMate(value))
            case "nps" => parts = parts.tail
            case "time" => parts = parts.tail
            case "pv" =>
              while (!parts.isEmpty) parts = parts.tail
            case _ =>
          }
        }
        if (depth != None && score != None)
          channel.foreach(_ ! Info(id, depth.get, score.get))
      case _ =>
    }
  }
}
