import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

/**
 * Created by hokada on 3/4/15.
 */
object MainSpec extends Properties("99 problems") {
  property("last") = forAll { (lst: List[Int]) => 
    lst match {
      case Nil => P01.last(lst).isEmpty
      case _ => P01.last(lst).get == lst.last
    }
  }
  
  property("penultimate") = forAll{ (lst: List[Int]) => 
    if (lst.size > 1) {
      P02.penultimate(lst).get == lst(lst.size - 2)
    } else {
      P02.penultimate(lst).isEmpty
    }
  }
  
  property("nth") = forAll{ (lst: List[Int], n: Int) =>
    if (0 <= n && n < lst.size) {
      P03.nth(n, lst).get == lst(n)
    } else {
      P03.nth(n, lst).isEmpty
    }
  }

  property("length") = forAll{ (lst: List[Int]) =>
    P04.length(lst) == lst.size
  }
  
  property("reverse") = forAll{ (lst: List[Int]) =>
    P05.reverse(lst) == lst.reverse
  }
  
  property("isPalindrome") = forAll{ (lst: List[Int]) =>
    P06.isPalindrome(lst:::lst.reverse)
  }
}
