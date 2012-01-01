package de.exoticorn.chess.base

case class Square private (private[base] val x: Int, private[base] val y: Int) {
  assert(x >= 0 && x < 8)
  assert(y >= 0 && y < 8)

  def file: Char = Square.files(x)
  def rank: Int = y + 1

  def -|(that: Square): Int = x - that.x
  def --(that: Square): Int = y - that.y

  def +?(d: (Int, Int)): Option[Square] = {
    val (dx, dy) = d
    val tx = x + dx
    val ty = y + dy
    if (tx >= 0 && tx < 8 && ty >= 0 && ty < 8) Some(new Square(tx, ty))
    else None
  }

  def rectTo(that: Square): Seq[Square] = {
    val minRank = y min that.y
    val maxRank = y max that.y
    val minFile = x min that.x
    val maxFile = x max that.x

    for (r <- minRank to maxRank; f <- minFile to maxFile) yield new Square(f, r)
  }

  def to(that: Square): Seq[Square] = {
    if (this == that)
      Seq(this)
    else if (y == that.y)
      for (nx <- x to that.x by (that.x - x).signum) yield new Square(nx, y)
    else if (x == that.x)
      for (ny <- y to that.y by (that.y - y).signum) yield new Square(x, ny)
    else {
      val (dx, dy) = (that.x - x, that.y - y)
      val d = dx.abs
      if (d == dy.abs) {
        val (sx, sy) = (dx.signum, dy.signum)
        for (i <- 0 to d) yield new Square(x + sx * i, y + sy * i)
      } else throw new Exception("Square.to called with target square that is neither straight nor exactly diagonal from this")
    }
  }

  def between(that: Square): Seq[Square] = (this to that).tail.init

  override def toString = ('a' + x).toChar.toString + (y + 1).toString
}

object Square {
  private val files = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

  def apply(file: Char, rank: Int): Square = new Square(files.indexOf(file), rank - 1)
  def apply(file: Symbol, rank: Int): Square = new Square(files.indexOf(file.name(0)), rank - 1)
  def all = Sq("a1") rectTo Sq("h8")
}

object Sq {
  implicit def apply(s: Symbol): Square = apply(s.name)

  def apply(s: String): Square = {
    val file = s(0)
    val rank = s(1).toString.toInt
    Square(file, rank)
  }
}

