package game

object Day8 {
  def maxInReg(input: String) = {
    maxInRegImpl(input)._2
  }

  def maxInRegDuring(input: String) = {
    maxInRegImpl(input)._1
  }

  // the same loop is used for both cases and returns a tuple of (maxInRegDuring, maxInReg)
  def maxInRegImpl(input: String) = {
    val ins = parseInstructions(input)
    // create the register from all keys.
    var register = ins.map(i => (i.k1, 0)).toMap
    var max = register.values.max
    ins.foreach(i => {
      // test if condition is true, then apply.
      if (checkOp(register.get(i.k2).get, i.op, i.cmp)) {
        register = register + (i.k1 -> (register.get(i.k1).get + i.add))
        max = Math.max(max, register.values.max)
      }
    })
    (max, register.values.max)
  }

  def checkOp(v: Int, op: String, cmp: Int): Boolean = {
    op match {
      case "==" => v == cmp
      case "!=" => v != cmp
      case ">" => v > cmp
      case ">=" => v >= cmp
      case "<" => v < cmp
      case "<=" => v <= cmp
    }
  }

  def parseInstructions(str: String) = {
    val lines = str.split("\n")
    lines.map(l => l.split("\\s+"))
      .map(l => Instruction(
        l(0),
        parseOp(l(1), l(2)),
        l(4),
        l(5),
        Integer.parseInt(l(6))))
      .toList
  }

  // simplify the inc / dec to a single integer.
  def parseOp(a: String, b: String): Int = {
    val i = Integer.valueOf(b)
    a match {
      case "inc" => i
      case "dec" => -1 * i
    }
  }

  case class Instruction(k1: String, add: Int = 0, k2: String = "", op: String, cmp: Int)

}
