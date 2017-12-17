package game

object Day17 {
  def spin(steps: Int) = {
    var a: Array[Int] = Array(0)
    var pos = 0
    (1 to 2017).foreach(i => {
      pos = (pos + steps) % i
      a = (a.take(pos + 1) :+ i) ++ a.drop(pos + 1)
      pos = pos + 1
    })
    a(pos + 1)
  }

  // we actually care only about position 1, which means, look out for any matches on pos 0.
  def spinFast(steps: Int) = {
    var pos = 0
    var ret = 1
    (1 to 50000000).foreach(i => {
      pos = (pos + steps) % i
      if (pos == 0) {
        ret = i
      }
      pos = pos + 1
    })
    ret
  }
}
