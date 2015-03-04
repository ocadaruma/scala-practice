import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

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
  
  property("compress") = forAll{ (lst: List[Int]) =>
    val l = P08.compress(lst)
    (0 until lst.size).grouped(2).forall { g =>
      if(g.size == 2) g(0) != g(1) else true
    }
  }
  
  property("pack") = forAll({ (lst: List[Int]) =>
    val l = P09.pack(lst)
    l.forall(innerList => innerList.forall(i => i == innerList.head))
  })
  
  property("decode") = forAll({ (lst: List[Int]) =>
    P12.decode(P10.encode(lst)) == lst
  })
  
  property("encodeDirect") = forAll({ (lst: List[Int]) =>
    P10.encode(lst) == P13.encodeDirect(lst)
  })
  
  property("duplicateN") = forAll({(lst: List[Int]) =>
    val dup =  P15.duplicateN(5, lst)
    dup.size == lst.size * 5
  })
}
