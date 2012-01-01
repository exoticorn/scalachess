package de.exoticorn.chess.uci

import de.exoticorn.chess.base._

class SyncEngine(command: String) {
  private val channel = new actors.Channel[Any]
  private val engine = new EngineCore(command)

  def quit() {
    engine ! Quit
  }

  private var position = Position.start

  def setPosition(pos: Position) {
    position = pos
  }

  def bestMove(time: Time): Move = {
    engine ! FindBestMove(channel, position, time)
    def receiveMove() = channel receive {
      case BestMove(_, m) => Some(m)
      case _ => None
    }
    def waitForMove(): Move = receiveMove() match {
      case Some(m) => m
      case None => waitForMove()
    }
    waitForMove()
  }

  def bestMoveWithInfo(time: Time): (Move, Info) = {
    engine ! FindBestMove(channel, position, time)
    var lastInfo: Option[Info] = None
    var bestMove: Option[Move] = None
    while (bestMove == None) channel receive {
      case info: Info => lastInfo = Some(info)
      case BestMove(_, m) => bestMove = Some(m)
      case _ =>
    }
    (bestMove.get, lastInfo.get)
  }

  override def toString = command
}