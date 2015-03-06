/**
 * Created by hokada on 3/2/15.
 */
sealed trait Node {
  val value: Int
}

class \^-^/[A]
object \^-^/ {
  implicit val ▂▅▇█▓▒░░▒▓█▇▅▂ = new \^-^/[Leaf]
  implicit def █▓▒░░▒▓█[A <: Node] = new \^-^/[Branch[A]]
}

case class Branch[+A <: Node](left: A, value: Int, right: A)(implicit ev: \^-^/[A]) extends Node
case class Leaf(value: Int) extends Node

case class BTree(node: Node) {
  def size(): Int = {
    def sizeIter(currentNode: Node): Int = currentNode match {
      case Leaf(v) => 1
      case Branch(l, _, r) => sizeIter(l) + sizeIter(r) + 1
    }

    val lll = Branch(Leaf(1),2,Leaf(3))
    val rrr= Branch(Branch(Leaf(5),6,Leaf(7)), 8, Branch(Leaf(9),10,Leaf(11)))
    val bbb=Branch(lll, 4, rrr)

    sizeIter(node)
  }

  def max(): Int = {
    def maxIter(currentNode: Node): Int = currentNode match {
      case Leaf(v) => v
      case Branch(_, _, r) => maxIter(r)
    }

    maxIter(node)
  }

  def min(): Int = {
    def minIter(currentNode: Node): Int = currentNode match {
      case Leaf(v) => v
      case Branch(l, _, _) => minIter(l)
    }

    minIter(node)
  }

  def avg(): Double = {
    def avgIter(currentNode: Node, count: Int, result: Int): (Int, Int) = currentNode match {
      case Leaf(v) => (count + 1, result + v)
      case Branch(l, v, r) => {
        val (leftCount, leftResult) = avgIter(l, count + 1, result + v)
        val (rightCount, rightResult) = avgIter(r, count + 1, result + v)
        (leftCount + rightCount, leftResult + rightResult)
      }
    }

    val (count, result) = avgIter(node, 0, 0)
    result.toDouble / count
  }

  def sum(): Int = {
    def sumIter(currentNode: Node): Int = currentNode match {
      case Leaf(v) => v
      case Branch(l, v, r) => sumIter(l) + v + sumIter(r)
    }

    sumIter(node)
  }

  def find(n: Int): Option[Node] = {
    def findIter(currentNode: Node): Option[Node] =  {
      currentNode match {
        case _ if currentNode.value == n => Some(currentNode)
        case Leaf(_) => None
        case Branch(l, v, r) => findIter(if (v < n) l else r)
      }
    }

    findIter(node)
  }
}

object T {
  val regex =
    ("""\s*(""" +
      """(//.*)|""" +
      """([0-9]+)|""" +
      """"(\\"|\\\\|\\n|[^"])*"|""" +
      """([A_Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\|\||\p{Punct})""" +
      """)?""").r

  def qsort(xs: Seq[Int]): Seq[Int] = xs match {
    case Nil | Seq(_) => xs
    case pivot +: tail =>
      val(smaller, greater) = tail.partition(_ <= pivot)
      qsort(smaller) ++ Seq(pivot) ++ qsort(greater)
  }

  def m(str: String) = str match {
    case regex(whole, comment, num, str, id) =>
      println(whole)
      println(comment)
      println(num)
      println(str)
      println(id)
    case _ => println("no match")
  }
}

