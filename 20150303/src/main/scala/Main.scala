/**
 * Created by hokada on 3/2/15.
 */
sealed trait Node {
  val value: Int
}

case class Branch(left: Node, value: Int, right: Node) extends Node
case class Leaf(value: Int) extends Node

case class BTree(node: Node) {
  def size(): Int = {
    def sizeIter(currentNode: Node): Int = currentNode match {
      case Leaf(v) => 1
      case Branch(l, _, r) => sizeIter(l) + sizeIter(r) + 1
    }

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
        case Branch(l, v, r) => {
          if (n < v) findIter(l) else findIter(r)
        }
      }
    }
    
    findIter(node)
  }
}
