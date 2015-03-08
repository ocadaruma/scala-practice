package com.mayreh.stone

/**
 * Created by hokada on 3/5/15.
 */

sealed trait Token {
  val lineNumber: Int
  val text: String
}

case class IdentifierToken(lineNumber: Int, text: String) extends Token
case class NumberToken(lineNumber: Int, text: String) extends Token {
  val number = text.toInt
}
case class StringToken(lineNumber: Int, text: String) extends Token
case object EOFToken extends Token {
  val lineNumber = -1
  val text = ""
}
