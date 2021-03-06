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

object P07 {
  def flatten(lst: List[Any]): List[Any] = {
    def flattenIter(l: List[Any], result: List[Any]): List[Any] = l match {
      case Nil => result
      case head::tail => head match {
        case ls: List[_] => flattenIter(tail, flattenIter(ls, Nil):::result)
        case _ => flattenIter(tail, head::result)
      }
    }

    P05.reverse(flattenIter(lst, Nil))
  }
}

object P08 {
  def compress[A](lst: List[A]): List[A] = {
    lst.foldRight(List[A]()) { (i, r) =>
      r match {
        case head::_ if head == i => r
        case _ => i :: r
      }
    }
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

object P16 {
  def drop[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def dropIter[A](index: Int, l: List[A], result: List[A]): List[A] = l match {
      case Nil => result
      case head :: tail => if ((index + 1) % n == 0) {
        dropIter(index + 1, tail, result)
      } else {
        dropIter(index + 1, tail, head :: result)
      }
    }

    P05.reverse(dropIter(0, lst, Nil))
  }
}

object P17 {
  def split[A](i: Int, lst: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitIter[A](j: Int, l: List[A], result: List[A]): (List[A], List[A]) = (j, l) match {
      case (0, _) => (P05.reverse(result), l)
      case (_, Nil) => (P05.reverse(result), l)
      case (_, head::tail) => splitIter(j-1, tail, head::result)
    }

    splitIter(i, lst, Nil)
  }
}

object P18 {
  def slice[A](from: Int, till: Int, lst: List[A]): List[A] = {
    @tailrec
    def sliceIter[A](index: Int, l: List[A], result: List[A]): List[A] = (index, l) match {
      case (_, Nil) => P05.reverse(result)
      case (i, _) if i >= till => P05.reverse(result)
      case (i, head::tail) =>
        if (from <= i && i < till) {
          sliceIter(index + 1, tail, head::result)
        } else {
          sliceIter(index + 1, tail, result)
        }
    }

    sliceIter(0, lst, Nil)
  }
}

object P19 {
  def rotate[A](i: Int, lst: List[A]): List[A] = {
    if (lst.size > 0) {
      val size = lst.size
      val mod = if (i < 0) i % size + size else i % size

      P18.slice(mod, mod + size, lst ::: lst)
    } else {
      Nil
    }
  }
}

object P20 {
  def removeAt[A](i: Int, lst: List[A]): (List[A], Option[A]) = {
    @tailrec
    def removeAtIter[A](j: Int, l: List[A], result: List[A]): (List[A], Option[A]) = (j, l) match {
      case (_, Nil) => (P05.reverse(result), None)
      case (_, head::tail) => if (j == i){
        (P05.reverse(result):::tail, Some(head))
      } else {
        removeAtIter(j + 1, tail, head::result)
      }
    }

    removeAtIter(0, lst, Nil)
  }
}

/* Arithmetic */

//▂▅▇█▓▒░(’ω’)░▒▓█▇▅▂うわあああああああ
object P31 {
  implicit class RichInt(n: Int) {
    def isPrime(): Boolean = n match {
      case _ if n <= 0 => throw new IllegalArgumentException
      case 1 => false
      case 2 => true
      case _ => (2 until n).forall(n % _ != 0)
    }
  }
}

object P32 {
  def gcd(m: Int, n: Int): Int = {
    val (smaller, greater) = if (m < n) (m , n) else (n, m)

    @tailrec
    def gcdIter(s: Int, g: Int): Int = {
      g % s match {
        case 0 => s
        case r => gcdIter(r, s)
      }
    }

    gcdIter(smaller, greater)
  }
}

object P33 {
  implicit class RichInt(n: Int) {
    def isCoprimeTo(m: Int): Boolean = {
      P32.gcd(n, m) == 1
    }
  }
}

object P34 {
  implicit class EulerInt(n: Int) {
    import P33._
    def totient(): Int = {
      (1 to n).count(_.isCoprimeTo(n))
    }
  }
}
