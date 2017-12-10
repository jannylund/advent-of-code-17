package game

object Day10 {

  def getSimpleHash(arr: Array[Int], len: List[Int]): Int = {
    val revhash = reverseHash(arr, len)
    revhash(0) * revhash(1)
  }

  def reverseHash(arr: Array[Int], len: List[Int], pos: Int = 0, skipSize: Int = 0): Array[Int] = {
    if(len.isEmpty) {
      arr
    } else {
      // get the sublist and reverse it. we use a double list to simulate the rotation.
      val rev = (arr ++ arr).slice(pos, (pos + len.head)).reverse
      // now create a new list that contains these new values as well as the incoming
      for((value, i) <- rev.view.zipWithIndex) {
        val index = (i + pos) % arr.length
        arr(index) = value
      }
      val nextPos = (pos + skipSize + len.head) % arr.length
      reverseHash(arr, len.drop(1), nextPos, skipSize + 1)
    }
  }

  def strToAscii(str: String): List[Int] = {
    str.map(c => c.toInt).toList
  }

  def strToAsciiWithSuffix(str: String): List[Int] = {
    strToAscii(str) ++ List(17, 31, 73, 47, 23)
  }

  // make hex and pad with zero
  def makeHex(arr: List[Int]): String = {
    arr.map(v => ("0" + v.toHexString) takeRight 2).reduceLeft(_ + _)
  }

  def denseHash(arr: Array[Int]): List[Int] = {
    arr.grouped(16).map(l => xor(l.toList)).toList
  }

  def xor(arr: List[Int]): Int = {
    arr.reduce((a, b) => a ^ b)
  }

  def getHash(str: String): String = {
    val lengths = strToAsciiWithSuffix(str)
    val arr = (0 to 255).toArray

    // make the lengths repeat 64 times.
    val large = List.fill(64)(lengths).flatten
    val revhash = reverseHash(arr, large)

    // split this in parts of 16 and get the densehash
    makeHex(denseHash(revhash))
  }
}
