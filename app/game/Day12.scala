package game

object Day12 {
  def countPrograms(str: String, id: Int) = {
    val m = getMap(str)
    rels(id, m).size
  }

  def countGroups(str: String) = {
    val m = getMap(str)
    groups(m).size
  }

  def groups(m: Map[Int, Set[Int]]): Set[Int] = {
    if (m.isEmpty) {
      Set()
    } else {
      val h = m.head._1
      val used = rels(h, m)
      Set(h) ++ groups(m.filterKeys(k => !used.contains(k)))
    }
  }

  def rels(id: Int, m: Map[Int, Set[Int]], acc: Set[Int] = Set.empty): Set[Int] = {
    val a = acc + id
    a ++ m.get(id).get.filterNot(i => a.contains(i)).map(i => rels(i, m, a)).flatten
  }

  def getMap(str: String): Map[Int, Set[Int]] = {
    val lines = str.replace(" ", "").split("\n")
    lines
      .map(l => l.split("<->"))
      .map({ case Array(key, values) => key.toInt -> values.split(",").map(_.toInt).toSet })
      .toMap
  }
}
