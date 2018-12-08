import scala.io.Source

class d1 {
  private val input = "input/input1"
  
  private def one(file: String): Int = {
    Source.fromFile(file).getLines.map(_.trim.toInt).fold(0)((a, b) => a+b)
  }
  
  private def two(file: String): Int = {
    var thisOne = Source.fromFile(file).getLines.map(_.trim.toInt).toArray
    var a: Array[Int] = Array(0)
    
    var f = a(0)
    do {
      for (i <- thisOne) {
        f += i
        if (a.contains(f)) return f
        else a = a.:+(f)
      }
      println(f)
    } while (true)
      
    return -1
  }
  
  private def other: Int = -13
  
  def execute(task: Int): Int = {
    task match {
      case 1 => one(input)
      case 2 => two(input)
      case _ => other
    }
  }
}