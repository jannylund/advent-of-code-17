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
}
