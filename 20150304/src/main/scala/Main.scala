import scala.annotation.tailrec

/**
 * Created by hokada on 3/2/15.
 * S-99: Ninety-Nine Scala Problems
 */

object P01 {
  @tailrec
  def last[A](lst: List[A]): Option[A] = lst match {
    case List(i) => Some(i)
    case _ :: rest => last(rest)
    case _ => None
  }
}

object P02 {
  @tailrec
  def penultimate[A](lst: List[A]): Option[A] = lst match {
    case List(i, _) => Some(i)
    case _::tail => penultimate(tail)
    case _ => None
  }
}

object P03 {
  @tailrec
  def nth[A](n: Int, lst: List[A]): Option[A] = (n, lst) match {
    case (_, Nil) => None
    case (0, head::_) => Some(head)
    case (_, _::tail) => nth(n-1, tail)
  }
}

object P04 {
  def length[A](lst: List[A]): Int = {
    @tailrec
    def lengthIter[A](lst: List[A], len: Int): Int = lst match {
      case Nil => len
      case _::tail => lengthIter(tail, len + 1)
    }
    
    lengthIter(lst, 0)
  }
}

object P05 {
  def reverse[A](lst: List[A]): List[A] = {
    @tailrec
    def reverseIter[A](l: List[A], result: List[A]): List[A] = l match {
      case Nil => result
      case head::tail => reverseIter(tail, head::result) 
    }
    
    reverseIter(lst, Nil)
  }
}

object P06 {
  def isPalindrome[A](lst: List[A]): Boolean = {
    lst == P05.reverse(lst)
  }
}

//object P07 {
//  def flatten[A](lst: List[Any]): List[A] = {
//    def flattenIter(l: List[Any], result: List[A]) = l match {
//      case Nil => result
//      case List(i) if i.isInstanceOf[A] =>
//    }
//  }
//}

object P08 {
  def compress[A](lst: List[A]): List[A] = {
    val result = lst.foldLeft(List[A]()) { (r, i) =>
      r match {
        case head::_ if head == i => r
        case _ => i :: r
      }
    }
    P05.reverse(result)
  }
}

object P09 {
  def pack[A](lst: List[A]): List[List[A]] = {
    val result = lst.foldLeft(List[List[A]]()) { (r, i) =>
      r match {
        case (innerLst @ (h::_))::tail if h == i => (i::innerLst)::tail
        case _ => List(i)::r
      }
    }
    P05.reverse(result)
  }
}

object P10 {
  def encode[A](lst: List[A]): List[(Int, A)] = {
    P09.pack(lst).map(innerList => (innerList.size, innerList.head))
  }
}

object P11 {
  def encodeModified[A](lst: List[A]) = {
    P09.pack(lst).map { innerList =>
      if (innerList.size > 1) {
        (innerList.size, innerList.head)
      } else {
        innerList.head
      }
    }
  }
}
    