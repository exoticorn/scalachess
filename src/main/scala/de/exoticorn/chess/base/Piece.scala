package de.exoticorn.chess.base

abstract class SquareContent
case class Piece(color: Symbol, piece: Symbol) extends SquareContent
case object EmptySquare extends SquareContent
