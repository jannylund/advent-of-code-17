package game

object Day5 {
  def solveMaze(input: String): Int = {
    val maze = input.split("\\s+").map(i => Integer.parseInt(i)).toList
    stepMaze(0, 0, maze)
  }

  def stepMaze(steps: Int, pos: Int, maze: List[Int]): Int = {
    if (pos < 0 || pos >= maze.size) {
      steps
    } else {
      val nextPos = pos + maze(pos)
      val nextMaze = maze.updated(pos, maze(pos) + 1)
      stepMaze(steps + 1, nextPos, nextMaze)
    }
  }

  def solveMaze2(input: String): Int = {
    val maze = input.split("\\s+").map(i => Integer.parseInt(i)).toList
    stepMaze2(0, 0, maze)
  }

  def stepMaze2(steps: Int, pos: Int, maze: List[Int]): Int = {
    if (pos < 0 || pos >= maze.size) {
      steps
    } else {
      val offset = maze(pos)
      val nextOffset = if (offset >= 3) {
        offset - 1
      } else {
        offset + 1
      }
      val nextMaze = maze.updated(pos, nextOffset)
      val nextPos = pos + offset
      stepMaze2(steps + 1, nextPos, nextMaze)
    }
  }
}
