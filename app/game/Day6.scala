package game

import play.api.Logger

object Day6 {

  def detectInfiniteLoop(blocks: List[Int]): Int = {
    var acc: List[List[Int]] = List()
    var localBlocks = blocks

    var steps = 0
    while (!acc.contains(localBlocks)) {
      steps += 1
      acc = acc :+ localBlocks
      localBlocks = balance(localBlocks.toArray)
    }
    Logger.debug("Found loop: " + localBlocks)
    steps
  }

  /** Find the amount of times before we are back at where we started **/
  def detectCountBetweenInfiniteLoops(blocks: List[Int]): Int = {
    val startBlocks = blocks
    var localBlocks = blocks
    var steps = 0
    do {
      localBlocks = balance(localBlocks.toArray)
      steps += 1
    } while (localBlocks != startBlocks)
    steps
  }

  // balance uses an array for faster updates when doing balancing.
  def balance(blocks: Array[Int]): List[Int] = {
    val size = blocks.size
    var distribute = blocks.max
    // indexof gives us automatically the first position if we got duplicates
    val index = blocks.indexOf(distribute)
    // reset to zero on position to distribute.
    blocks(index) = 0

    val share = (math.ceil(distribute.toDouble / size)).toInt
    for (i <- ((index + 1) to (index + size))) {
      val li = i % size
      if (distribute > 0) {
        blocks(li) += math.min(share, distribute)

        if (distribute >= share) {
          distribute = distribute - share
        } else {
          distribute = 0
        }
      }
    }
    blocks.toList
  }
}
