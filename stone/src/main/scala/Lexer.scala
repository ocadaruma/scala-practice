package com.mayreh.stone

import java.io.{LineNumberReader, Reader}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by hokada on 3/5/15.
 */
object Lexer {
  private val commentRegex = """//.*"""
  private val numberRegex = """[0-9]+"""
  private val stringRegex = """"(?:\\"|\\\\|\\n|[^"])*""""
  private val identifierRegex = """[A_Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\|\||\p{Punct}"""

  val regex = s"""(\\s*(?:($commentRegex)|($numberRegex)|($stringRegex)|($identifierRegex))?)""".r

  def apply(reader: Reader): Lexer = {
    new Lexer(reader)
  }
}

class Lexer(reader: Reader) {
  private val lineNumReader = new LineNumberReader(reader)
  private val queue = mutable.Queue[Token]()
  private var hasMore = true

  def read(i: Int = 0): Token = if (fillQueue(i)) queue.dequeue else EOFToken

  @tailrec
  private def fillQueue(i: Int): Boolean = {
    if (i < queue.size) {
      true
    } else if (!hasMore) {
      false
    } else {
      readLine
      fillQueue(i)
    }
  }

  private def readLine(): Unit = {
    import Lexer.regex

    val line = lineNumReader.readLine()
    if (line != null) {
      val lineNum = lineNumReader.getLineNumber

      queue ++= regex.findAllMatchIn(line).collect({
        case <>(None, _*) => throw new ParseException(s"syntax error at line:$lineNum")
        case <>(_, None, Some(num), _*) => NumberToken(lineNum, num)
        case <>(_, None, _, Some(str), _*) => StringToken(lineNum, str)
        case <>(_, None, _, _, Some(id), _*) => IdentifierToken(lineNum, id)
      })
    } else { hasMore = false }
  }
}
