package common

class CircularList[T] {
  case class Node(var element: T) {
    var next: Node = null
    var prev: Node = null
  }

  private var head: Node = null
  private var current = head

  def insert(e: T): Unit = {
    if (head == null) {
      head = Node(e)
      head.next = head
      head.prev = head
      current = head
    } else {
      val newNode = Node(e)
      newNode.next = current.next
      newNode.prev = current
      current.next.prev = newNode
      current.next = newNode
      current = newNode
    }
  }

  def remove(): T = {
    val currentValue = apply()
    current.prev.next = current.next
    current.next.prev = current.prev
    current = current.next
    currentValue
  }

  def move(offset: Int): Unit = {
    var i = offset
    while (i < 0) {
      current = current.prev
      i += 1
    }
    while (i > 0) {
      current = current.next
      i -= 1
    }
  }

  def apply(): T = current.element

  override def toString: String = {
    var t = head
    var s = ""
    do {
      s += t.element + " "
      t = t.next
    } while (t != head)
    s
  }
}
