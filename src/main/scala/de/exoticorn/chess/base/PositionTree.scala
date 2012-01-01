package de.exoticorn.chess.base

class PositionTree(val position: Position, val children: List[PositionTree]) {
  def +(that: PositionTree): PositionTree = {
    assert(that.position == position)
    val thisChildren = for (ch <- children) yield (ch /: that.children.filter(_.position == ch.position))(_ + _)
    val thatChildren = that.children.filterNot(ch => children.exists(_.position == ch.position))
    new PositionTree(position, thisChildren ::: thatChildren)
  }

  override def toString = mkString(true)

  private def mkString(forceNumber: Boolean): String = {
    children match {
      case Nil => " "
      case xs :: rest =>
        val moveNumber = if (position.turn == 'white) position.move.toInt + ". "
        else if (forceNumber) position.move.toInt + ".."
        else ""
        val moveString = xs.position.lastMove.get.toString
        val variations = for (ch <- rest) yield {
          val moveNumber = "(%d.%c".format(ch.position.move.toInt, (if (ch.position.turn == 'white) ' ' else '.'))
          val rest = ch.mkString(false)
          moveNumber + " " + ch.position.lastMove.get.toString + (if (rest.isEmpty) "" else " " + rest) + ") "
        }.mkString
        moveNumber + moveString + " " + variations.mkString + xs.mkString(false)
    }
  }
}

object PositionTree {
  def apply(position: Position): PositionTree = {
    var pos = position
    var tree = new PositionTree(pos, Nil)
    while (pos.lastMove != None) {
      pos = pos.lastMove.get.pos
      tree = new PositionTree(pos, List(tree))
    }
    tree
  }
}