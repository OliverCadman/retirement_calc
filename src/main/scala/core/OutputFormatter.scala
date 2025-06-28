package core

trait OutputFormatter {

  def normaliseCurrency(currString: String): String = {
    currString.replaceAll("(?<=\\d)(?=(\\d{3})+(?!\\d))", ",")
  }
}
