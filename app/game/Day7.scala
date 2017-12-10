package game

import play.api.Logger

object Day7 {
  def findRoot(input: String): String = {
    val tree = buildTreeList(input)
    val allChildren = tree.flatMap(t => t.children).toSet
    tree
      .filter(t => !t.children.isEmpty)
      .filter(t => !allChildren.contains(t.name))
      .head
      .name
  }

  def findBalance(str: String) = {
    findProperWeightOfUnbalanced(buildTree(str))
  }

  def findProperWeightOfUnbalanced(tower: Day7.Tower): Int = {
    // if children are unbalanced but all grandchildren are balanced, we know we found the one to fix.
    if (areChildrenUnbalanced(tower) && tower.children.filter(c => areChildrenUnbalanced(c)).isEmpty) {
      getRightWeight(tower)
    } else {
      tower.children.map(c => {
        findProperWeightOfUnbalanced(c)
      }).sum
    }
  }

  def areChildrenUnbalanced(tower: Tower): Boolean = {
    val weights = tower.children.map(t => getWeight(t))
    val balance = weights.isEmpty || weights.distinct.size == 1
    !balance
  }

  def getWeight(tower: Tower): Int = {
    tower.weight + tower.children.map(c => getWeight(c)).sum
  }

  // this works by grouping the weights together and comparing the single one with the ones used in many places.
  def getRightWeight(tower: Tower): Int = {
    val weights = tower.children.map(t => getWeight(t))
    val ws = weights.groupBy(identity).mapValues(_.size).toSeq.sortBy(_._2)
    val weightIs = ws.head._1
    val weightShould = ws.reverse.head._1
    val diff = weightShould - weightIs
    val badTower = tower.children.filter(t => getWeight(t) == weightIs).head
    (badTower.weight + diff)
  }

  def buildTree(str: String): Tower = {
    val treeMap = buildTreeList(str).map(t => (t.name -> t)).toMap
    val root = findRoot(str)
    val t = treeMap.get(root).get
    Tower(t.name, t.weight, getChildren(t.name, treeMap))
  }

  def getChildren(name: String, acc: Map[String, TowerSimple]): List[Tower] = {
    val t = acc.get(name).get
    t.children
      .map(c => acc.get(c).get)
      .map(c => Tower(c.name, c.weight, getChildren(c.name, acc)))
  }

  def buildTreeList(input: String): List[TowerSimple] = {
    val ts = input.split("\n").toList
    val towers = ts
      .map(t => t.split("\\s+"))
      .map(ta => {
        val name = ta(0)
        val weight = Integer.parseInt(ta(1).replace("(", "").replace(")", ""))
        val children = {
          if (ta.size > 3) {
            ta.drop(3).map(c => c.replace(",", "")).toList
          } else {
            List.empty
          }
        }
        TowerSimple(name, weight, children)
      })
    towers
  }

  case class TowerSimple(name: String, weight: Int, children: List[String] = List.empty)

  case class Tower(name: String, weight: Int, children: List[Tower] = List.empty)

}
