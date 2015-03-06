import java.io.{LineNumberReader, Reader}

import scala.collection.mutable

/**
 * Created by hokada on 3/5/15.
 */
object Lexer {
  private val commentRegex = """//.*"""
  private val numberRegex = """[0-9]+"""
  private val stringRegex = """"(?:\\"|\\\\|\\n|[^"])*""""
  private val identifierRegex = """[A_Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\|\||\p{Punct}"""

  val regex = s"""\s*(($commentRegex)|($numberRegex)|($stringRegex)|$identifierRegex)?""".r

  def apply(reader: Reader): Lexer = {
    new Lexer(reader)
  }
}

class Lexer(reader: Reader) {
  private val lineNumReader = new LineNumberReader(reader)
  private val queue = mutable.Queue[Token]()

//  def read(): Token = if (fillQueue(0)) queue.dequeue else EOFToken

//  private def fillQueue(i: Int): Boolean = {
//  }

  private def readLine(): Unit = {
    import Lexer.regex

    val line = lineNumReader.readLine()
    if (line != null) {
      val lineNum = lineNumReader.getLineNumber

      def readIter(startPos: Int): Unit = {
        if (startPos < line.length) {
        }
      }
      line match {
        case regex(whole,_*) if whole == null => None
        case regex(_,comment,_*) if comment != null => None
        case regex(whole,_,num,_*) if num != null => Some(NumberToken(lineNum, whole))
        case regex(whole,_,_,str,_*) if str != null => Some(StringToken(lineNum, whole))
        case regex(whole,_*) if whole != null => Some(IdentifierToken(lineNum, whole))
        case _ => None
      }
    }
  }
}
