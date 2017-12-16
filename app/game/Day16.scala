package game

object Day16 {
  def getMoves(in: String): Seq[String] = {
    in.split(",")
  }

  def dance(in: Seq[Char], moves: Seq[String]): Seq[Char] = {
    var team = in
    (0 to moves.size - 1).foreach(i => {
      team = move(team, moves(i))
    })
    team
  }

  def move(team: Seq[Char], move: String): Seq[Char] = {
    move.head match {
      case 's' => {
        val pos = move.tail.toInt
        team.takeRight(pos) ++ team.dropRight(pos)
      }
      case 'x' => {
        val p = move.tail.split("/")
        val a = p(0).toInt
        val b = p(1).toInt
        team.updated(a, team(b)).updated(b, team(a))
      }
      case 'p' => {
        val p = move.tail.split("/")
        val pA = team.indexOf(p(0)(0))
        val pB = team.indexOf(p(1)(0))
        team.updated(pA, team(pB)).updated(pB, team(pA))
      }
    }
  }

  /**
    * our input is smaller than repetitions, so we are likely to find loops as we keep running the same moves.
    */
  def repeatDance(start: Seq[Char], moves: Seq[String], repeat: Int) = {
    val cache = buildCache(start, moves, repeat)
    if (cache.size >= repeat) {
      cache(repeat - 1)
    } else {
      cache((repeat % cache.size) - 1)
    }
  }

  def buildCache(in: Seq[Char], moves: Seq[String], repeat: Int) = {
    var cache: Array[Seq[Char]] = Array()
    var foundLoop = false
    var team = in
    while (cache.size < repeat && !foundLoop) {
      team = dance(team, moves)
      if (!cache.isEmpty && team == cache.head) {
        foundLoop = true
      } else {
        cache = cache :+ team
      }
    }
    cache
  }
}