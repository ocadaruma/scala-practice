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
    lst.foldRight(List[List[A]]()) { (i, acc) =>
      acc match {
        case (innerLst@(h :: _)) :: tail if h == i => (i :: innerLst) :: tail
        case _ => List(i) :: acc
      }
    }
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

object P12 {
  def decode[A](lst: List[(Int, A)]): List[A] = {
    lst.foldRight(List[A]()){(t, acc) =>
      val (n, e) = t
      List.fill(n)(e):::acc
    }
  }
}

object P13 {
  def encodeDirect[A](lst: List[A]): List[(Int, A)] = {
    lst.foldRight(List[(Int, A)]()) { (i, acc) =>
      acc match {
        case (n, e) :: tail if e == i => (n + 1, e) :: tail
        case _ => (1, i) :: acc
      }
    }
  }
}

object P14 {
  def duplicate[A](lst: List[A]): List[A] = {
    P15.duplicateN(2, lst)
  }
}

object P15 {
  def duplicateN[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def duplicateIter[A](m: Int, l: List[A], result: List[A]): List[A] = l match {
      case Nil => result
      case head::tail => m match {
        case 0 => duplicateIter(n, tail, result)
        case x if x > 0  => duplicateIter(m-1, l, head::result)
        case _ => throw new IllegalArgumentException("n must greater than 0")
      }
    }

    P05.reverse(duplicateIter(n, lst, Nil))
  }
}