package y2018.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day8 extends Day(inputPath(2018, 8)) {
  private val data: Array[Int] = using(Source.fromResource(inputs(0)))(_.getLines().next().split(' ').map(_.toInt))

  case class Node(children: Array[Node], metadata: Array[Int])

  def processNode(from: Int): (Node, Int) = {
    var nodeCount = data(from)
    val children = new Array[Node](nodeCount)
    var offset = 2
    while (nodeCount > 0) {
      val nodeResult = processNode(from+offset)
      children(children.length-nodeCount) = nodeResult._1
      offset += nodeResult._2
      nodeCount -= 1
    }
    val metadata = data.slice(from+offset, from+offset+data(from+1))
    (Node(children, metadata), offset+data(from+1))
  }

  private val root = processNode(0)._1

  def one: Int = {
    def sumMetadata(node: Node): Int = {
      (if (node.children.isEmpty) 0
      else node.children.map(sumMetadata).sum) + node.metadata.sum
    }
    sumMetadata(root)
  }

  def two: Int = {
    def value(node: Node): Int = {
      if (node.children.isEmpty) node.metadata.sum
      else {
        node.metadata.map(num => {
          val index = num - 1
          if (index < 0 || index >= node.children.length) 0
          else value(node.children(index))
        }).sum
      }
    }
    value(root)
  }
}
