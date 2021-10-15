package y2018

object ElfCode {
  def runInstruction(instructions: Array[String], ip_bound: Int, registers: Array[Int], ip: Int): Int = {
    val opcode::operandList = instructions(ip).split(' ').toList
    val operands = operandList.map(_.toInt).toArray
    registers(ip_bound) = ip
    val value = opcode match {
      case "addr" => registers(operands(0)) + registers(operands(1))
      case "addi" => registers(operands(0)) + operands(1)
      case "mulr" => registers(operands(0)) * registers(operands(1))
      case "muli" => registers(operands(0)) * operands(1)
      case "banr" => registers(operands(0)) & registers(operands(1))
      case "bani" => registers(operands(0)) & operands(1)
      case "borr" => registers(operands(0)) | registers(operands(1))
      case "bori" => registers(operands(0)) | operands(1)
      case "setr" => registers(operands(0))
      case "seti" => operands(0)
      case "gtir" => if (operands(0) > registers(operands(1))) 1 else 0
      case "gtri" => if (registers(operands(0)) > operands(1)) 1 else 0
      case "gtrr" => if (registers(operands(0)) > registers(operands(1))) 1 else 0
      case "eqir" => if (operands(0) == registers(operands(1))) 1 else 0
      case "eqri" => if (registers(operands(0)) == operands(1)) 1 else 0
      case "eqrr" => if (registers(operands(0)) == registers(operands(1))) 1 else 0
      case _ => 0
    }
    registers(operands(2)) = value
    registers(ip_bound) + 1
  }
}
