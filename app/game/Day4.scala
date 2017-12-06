package game

object Day4 {

  def countValidPassPhrases(phrases: List[String]): Int = {
    phrases.filter(p => validatePassPhrase(p)).size
  }

  def validatePassPhrase(input: String): Boolean = {
    val words = input.split("\\s+")
    words.size == words.distinct.size
  }

  def countValidPassPhraseAnagrams(phrases: List[String]): Int = {
    phrases.filter(p => validatePassPhraseAnagram(p)).size
  }

  def validatePassPhraseAnagram(input: String): Boolean = {
    val words = input.split("\\s+").map(s => s.sorted)
    words.size == words.distinct.size
  }
}
