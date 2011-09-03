package org.beeherd.games;

class Result(val board: Board)

case class GoodPlay(
  override val board: Board
  , val createdWords: List[String]
) extends Result(board) {
  lazy val score = createdWords.foldLeft (0) {(sum, w) => wordScore(w) + sum}
  def wordScore(word: String): Int = 1;
}

case class BadPlay(
  override val board: Board
  , val violation: String
) extends Result(board)


/*
private[this] object BoardHelper {
  lazy val mutableRows(dim: Int) = {
    import scala.collection.mutable._;
    val newMutableRows = new ArrayBuffer[ArrayBuffer[Char]]();
    var x = 0;
    while (x < dim) {
      val row = new ArrayBuffer[Char]();
      var y = 0;
      while (y < dim) {
        row += ' '
        y = y + 1;
      }
      newMutableRows += row;
      x = x + 1;
    }
    newMutableRows
  }
}
*/

case class Board(
  val rows: List[List[Char]]
  , val dictionary: Dictionary = new SimpleDictionary()
) {

  {
    /*
     * Boards get created like crazy.  Who needs validation?? Protect by giving
     * no public access to construction.
    if (rows == null || rows.size == 0)
      throw new IllegalArgumentException("There must be at least one row.");
    if (rows.size % 2 == 0)
      throw new IllegalArgumentException("Both dimensions must be odd."); 
    if (rows.exists {_.size != rows.size})
      throw new IllegalArgumentException("The dimensions of the board must equal.");
     */
  }

  lazy val gameStartingPoint = (0, 0)
  lazy val numRows = rows.size;
  lazy val numColumns = rows(0).size;
  lazy val isEmpty = !rows.exists {r => r.exists {c => c != ' '}}

  def print: Unit = {
    val numRows = rows.size;
    val numColumns = rows(0).size;
    val indent = 1;
    rows.zipWithIndex.foreach {r => 
      val row = r._1;
      val rowNum = r._2;
      if (rowNum == 0) {
        Console.print("   ");
        row.zipWithIndex.foreach {c => Console.print(c._2 % 10 + " ")}
        Console.println();
      }

      println("  " + ("--" * indent * numColumns) + "-")
      Console.print(rowNum % 10 + " |");
      row.foreach {c => Console.print(c + "|")}
      println();
    }
    println("  " + ("--" * indent * numColumns) + "-")
  }

  // not good
  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Board])
      return false;

    val thatRows = that.asInstanceOf[Board].rows;

    if (rows.size != thatRows.size)
      return false;

    rows.zipWithIndex.forall {
      r => r._1.zipWithIndex.forall {c => c._1 == thatRows(r._2)(c._2)}
    }
  }

  private def acrossNoQuestions(letters: String, x: Int, y: Int): Board = {
    // find the index of the blanks that we'll be filling with letters
    val row = rows(x);

    def helper(word: String, index: Int): List[Char] = {
      // Where's my pattern matching???
      if (word.length == 0) {
        List[Char]();
      } else if (row(index) != ' ') {
        row(index) :: helper(word, index + 1);
      } else if (word.length == 1) {
        List(word(0))
      } else {
        word(0) :: helper(word.substring(1), index + 1)
      }
    }

    val left = row.take(y)
    val middle = helper(letters, y);
    val right = row.drop(left.size + middle.size);
    val newRow = left ++ middle ++ right;
    new Board(rows.take(x) ++ List(newRow) ++ rows.drop(x + 1), dictionary);
  }

  /**
  * Attempt to play the letters across starting on row x and column y
  * 
  * @param letters  the letters to play (in order)
  * @param x        row (0-based)
  * @param y        column (0-based)
  * @return         the result
  */
  def across(letters: String, x: Int, y: Int): Result = {
    if (letters == null) 
      return BadPlay(this, "The letters must be specified.");
    
    if (!letters.forall {c => Character.isLetter(c)}) 
      return BadPlay(this, "All the characters of the letters must be letters.");

    if (x < 0 || y < 0) 
      return BadPlay(this, "Starting point must be positive.");

    // This isn't a good test
    if (y + letters.length > numRows)
      return BadPlay(this, letters + " does not fit on the board.");

    // If this is the first move (the board is empty), the only caveat is that
    // it must have a char on the starting spot.
    if (isEmpty) {
      val potential = acrossNoQuestions(letters, x, y);
      val center = rows.size / 2
      if (potential.rows(center)(center) == ' ')
        return BadPlay(this, "The first play must cross the center of the board.");
      else
        return GoodPlay(potential, List(letters));
    }

    if (rows(x)(y) != ' ') 
      return BadPlay(this, "A character already exists on the starting point.");

    val potential = acrossNoQuestions(letters, x, y);

    // For this to be a legal move it must not attempt to overwrite any existing
    // letterss.
    // This is dumb, just see what the coords are of what you are going to put
    // down and see if any chars fall on any of them.
    // I don't think this check is necessary any more
//--------------------------------------------------
//     if ((potential.charsOnBoard - this.charsOnBoard) != letters.length) 
//       return (this, Some("You may not take existing characters off the board."));
//-------------------------------------------------- 

    // Furthermore it must create words out of any letters that it touches
    val createdWords = potential.createdWords(letters, x, y);
    
    if (createdWords.size == 0)
      return BadPlay(this, "You did not create any words.");
    
    val badWord = createdWords.find {w => !dictionary.isWord(w)};
    if (!badWord.isEmpty) 
      return BadPlay(this, "Your play created the invalid word, " + badWord.get + ".");

    return GoodPlay(potential, createdWords);

  }

  /**
  * Attempt to play the letters down starting on row x and column y
  * 
  * @param letters  the letters to play (in order)
  * @param x        row (0-based)
  * @param y        column (0-based)
  * @return         the result
  */
  def down(letters: String, x: Int, y: Int): Result = {
    val result = flip.across(letters, y, x);
    result match {
      case GoodPlay(b, w) => GoodPlay(b.flip, w)
      case BadPlay(b, m) => BadPlay(b.flip, m);
    }
  }

  // Leaves the characters on the supplied board that do not appear on this board.
  def --(that: Board): Board = {
    var ctr = -1;
    val paired = rows.map {r => 
      ctr = ctr + 1;
      r.zip(that.rows(ctr));
    }
    val newRows = paired.map {r =>
      r.map {p => if (p._1 == p._2) '~' else p._1}
    }
    new Board(newRows, dictionary);
  }

  /**
  * Find the horizontal word that crossed the cell with coordinates (x,y).  The
  * word is guaranteed to be valid, but only because the code currently
  * guarantees the board does not contain invalid words (at least it's supposed
  * to!)
  * 
  * @param x  row (0-based)
  * @param y  column (0-based)
  * @return   the word formed
  */
  def word(x: Int, y: Int): String = {
    // you can make this faster.  Of course, almost all of this could be...
    if (rows(x)(y) == ' ')
      return " ";

    val leftSide = rows(x).take(y)
      .reverse.takeWhile{c => Character.isLetter(c)}.reverse;
    val rightSide = rows(x).drop(y + 1).takeWhile{c => Character.isLetter(c)};
    leftSide.mkString + rows(x)(y) + rightSide.mkString
  }

  /**
  * Find all words created by playing the supplied letters horizontally
  * starting on row x and column y.  The word is guaranteed to be valid, but
  * only because the code currently guarantees the board does not contain
  * invalid words (at least it's supposed to!)
  *
  * @param letters the letters to use (in order)
  * @param x       row (0-based)
  * @param y       column (0-based)
  * @return        the words formed
  */
  def createdWords(letters: String, x: Int, y: Int): List[String] = {
    // First, we find the word that goes down,
    val verticalWord = this.flip.word(y, x);
    // make sure you don't go off the board
    val r = x until letters.size + x;
    val horizontalWords = r.map {i => word(i, y)}
    val filtered = horizontalWords.filter {_.size != 1}.toList
    if (verticalWord.length == 1)
      filtered
    else
      verticalWord :: filtered.toList
  }

  // This works because the board is square.
  lazy val flip: Board = {
    val dim = rows.size;
    /* Too slow.  This guy gets called *a lot* by BoardAnalyzer
    val newRows = (0 until x).foldLeft (List[List[Char]]()) {(newRows, i) => {
      // todo count down and use rows(j)(i) :: cols
        newRows ++ List((0 until y).foldLeft (List[Char]()) {(cols, j) => cols ++ List(rows(j)(i))})
      }
    }
    */

    import scala.collection.mutable._;

    /*
    val newMutableRows = new ArrayBuffer[List[Char]]();
    (0 until x).foreach {i => newMutableRows += (0 until y).map {j => rows(j)(i)}.toList}
    val newRows = rows.toList;
    
    */
    val newMutableRows = new ArrayBuffer[ArrayBuffer[Char]]();
    var x = 0;
    while (x < dim) {
      val row = new ArrayBuffer[Char]();
      var y = 0;
      while (y < dim) {
        row += rows(y)(x);
        y = y + 1;
      }
      newMutableRows += row;
      x = x + 1;
    }
    val newRows = newMutableRows.map {_.toList}.toList

    new Board(newRows, dictionary);
  }

  lazy val charsOnBoard: Int = {
    rows.foldLeft (0) {(sum, r) => 
    sum + r.foldLeft (0) {(sum, c) => if (Character.isLetter(c)) sum + 1 else sum}}
  }
}

case class Play(
  val letters: List[Char]
  , val row: Int
  , val column: Int
  , val result: Result
)

object Game {
  def createBoard(dim: Int, dict: Dictionary = new SimpleDictionary()): Board = {
    val x = dim;
    val y = dim;
    val rows = (0 until x).foldLeft (List[List[Char]]()) {(rows, num) => {
        rows ++ List((0 until y).foldLeft (List[Char]()) {(cols, _) => ' ' :: cols})
      }
    }
    new Board(rows, dict);
  }
}

class Game(boardSize: Int) {
  def f() = {
    println("hsi");
  }
  val plays = new scala.collection.mutable.ArrayStack[Board]();

  def start = plays += Game.createBoard(boardSize);

  def down(word: String, x: Int, y: Int): Board = {
    if (plays.size == 0)
      start;
    val result = plays.peek.down(word, x, y);
    result match {
      case BadPlay(_, msg) => println(msg);
      case _ => {}
    }
    val newBoard = result.board;
    plays += newBoard;
    newBoard.print;
    newBoard;
  }

  def across(letters: String, x: Int, y: Int): Board = {
    if (plays.size == 0)
      start;
    val result = plays.peek.across(letters, x, y);
    result match {
      case BadPlay(_, msg) => println(msg);
      case _ => {}
    }
    val newBoard = result.board;
    plays += newBoard;
    newBoard.print;
    newBoard;
  }
}
