import scala.annotation.tailrec

/**
 * Created by hokada on 3/2/15.
 * S-99: Ninety-Nine Scala Problems
 */

object P01 {
  @tailrec
  def last[T](lst: List[T]): Option[T] = lst match {
    case List(i) => Some(i)
    case _ :: rest => last(rest)
    case _ => None
  }
}

object P02 {
  @tailrec
  def penultimate[T](lst: List[T]): Option[T] = lst match {
    case List(i, _) => Some(i)
    case _::tail => penultimate(tail)
    case _ => None
  }
}

object P03 {
  @tailrec
  def nth[T](n: Int, lst: List[T]): Option[T] = (n, lst) match {
    case (_, Nil) => None
    case (0, head::_) => Some(head)
    case (_, _::tail) => nth(n-1, tail)
  }
}

object P04 {
  def length[T](lst: List[T]): Int = {
    @tailrec
    def lengthIter[T](lst: List[T], len: Int): Int = lst match {
      case Nil => len
      case _::tail => lengthIter(tail, len + 1)
    }
    
    lengthIter(lst, 0)
  }
}

object P05 {
  def reverse[T](lst: List[T]): List[T] = {
    @tailrec
    def reverseIter[T](l: List[T], result: List[T]): List[T] = l match {
      case Nil => result
      case head::tail => reverseIter(tail, head::result) 
    }
    
    reverseIter(lst, Nil)
  }
}

object P06 {
  def isPalindrome[T](lst: List[T]): Boolean = {
    lst == P05.reverse(lst)
  }
}
//
//object P07 {
//  def flatten(lst: List[Any]): List[Any] = {
//    def flattenIter(l: List[Any], result: List[Any]) = l match {
//      case
//    }
//  }
//}
//
