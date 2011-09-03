package org.beeherd.games

object Driver {
  def main(args: Array[String]): Unit = {
    val b = Game.createBoard(5);
    val moves = List(
      ("hi", (2,2))
      , ("s", (4,2))
      , ("of", (2,3))
    )
    var temp = b; // todo: do this without variable
    val boards = moves.map {m => temp = temp.down(m._1, m._2._1, m._2._2).board; temp}
    boards.foreach {_.print};

    boards(2).createdWords("h", 2,2).foreach {w => println(w)}
  }
}
