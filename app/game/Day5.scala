package game

object Day5 {
  def solveMaze(input: String): Int = {
    val maze = input.split("\\s+").map(i => Integer.parseInt(i))
    stepMaze(0, 0, maze)
  }

  def stepMaze(steps: Int, pos: Int, maze: Array[Int]): Int = {
    if (pos < 0 || pos >= maze.size) {
      steps
    } else {
      val nextPos = pos + maze(pos)
      maze(pos) = maze(pos) + 1
      stepMaze(steps + 1, nextPos, maze)
    }
  }

  def solveMaze2(input: String): Int = {
    val maze = input.split("\\s+").map(i => Integer.parseInt(i))
    stepMaze2(0, 0, maze)
  }

  def stepMaze2(steps: Int, pos: Int, maze: Array[Int]): Int = {
    if (pos < 0 || pos >= maze.size) {
      steps
    } else {
      val offset = maze(pos)
      val nextOffset = if (offset >= 3) {
        offset - 1
      } else {
        offset + 1
      }
      maze(pos) = nextOffset
      val nextPos = pos + offset
      stepMaze2(steps + 1, nextPos, maze)
    }
  }
}
