/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.beeherd.games

class BoardAnalyzer {

  def availablePlays(
    board: Board
    , letters: List[Char]
    , combos: List[List[Char]])
  : List[Play] = {
    val range = 0 until board.rows.length;
    range.flatMap {i => 
      //playsAcross(board, i, letters, combos) ++
      playsDown(board, i, letters, combos)
    }.toList
  }

  /**
  * Find all words that can be played on the column using the supplied letters.
  *
  * @param board   the board to analysize
  * @param column  column upon which you want to make words
  * @param letters available letters that you can use with blanks
  */
  def playsDown(
    board: Board
    , column: Int
    , letters: List[Char]
    , combos: List[List[Char]]
  ): List[Play] = {
    playsAcross(board.flip, column, letters, combos)
    .map {p => Play(p.letters, p.column, p.row, p.result)}
  }

  /**
  * Find all words that can be played on the row using the supplied letters.
  * @param board   the board to analysize
  * @param row     row upon which you want to make words
  * @param letters available letters that you can use with blanks
  */
  def playsAcross(
    board: Board
    , rowNum: Int
    , letters: List[Char]
    , combos: List[List[Char]]
  ): List[Play] = {
    val row = board.rows(rowNum);
    val pivots = findPivots(row);

    // TODO, consider using Larry's approach of starting from the dictionary as
    // opposed to throwing all combos down and asking if it is a word.

    // This is the algorithm I'm using:
    //
    // From a pivot go as far left as you can (governed by blanks, boundaries,
    // and available letters.  For each move left, you try all your letters, if
    // one doesn't make a word down, you can throw it away until you are making
    // words on spaces that do not include the current one.  Once you reach the
    // far left and calculate all words, go as far right as you can, excluding
    // words that only reach the pivot (those have been covered by going left).
    // Move one blank right and repeat until you reach the pivot.  At the
    // pivot, go as far right as you can.  When that is done, move to the next
    // pivot.  Repeat process until the pivots are done.

    val numLetters = letters.size;
    // If we have more letters than blanks between two pivots , we only want to
    // go left as far as (numBlanks between pivots - 1) because the
    // numBlanks-left case for the trailing pivot is covered by the goRight for
    // the preceding one.
    val plays = pivots.flatMap {p =>
      val pivotStart = p._1;
      val blanksBetweenPivots = p._2;
      val stoppingIndex =
        if (blanksBetweenPivots == 0)
          0
        else
          pivotStart - blanksBetweenPivots - 1

      val left = goLeft(board, rowNum, letters, combos, pivotStart, stoppingIndex);

      val rightPt = {
        var i = pivotStart + 1;
        val len = row.length;
        while (i < len && row(i) != ' ') {
          i = i + 1;
        }
        i
      }
      val right = goRight(board, rowNum, letters, combos, rightPt);
      left ++ right
    }
    plays
  }

  // Find the starting point of existing plays on some row.  The distance from
  // the last pivot is included as the second part of the pair.
  def findPivots(row: List[Char]): List[(Int, Int)] = {
    val pivots = new scala.collection.mutable.ArrayBuffer[(Int, Int)]();
    var lastCharOfLastPivot = -1;
    var ctr = 0;
    while (ctr < row.size) {
      if (row(ctr) != ' ') {
        if (ctr == 0 || row(ctr - 1) == ' ') {
          if (lastCharOfLastPivot != -1)
            pivots += ((ctr, ctr - lastCharOfLastPivot - 1));
          else
            pivots += ((ctr, 0));
        } else {
          lastCharOfLastPivot = ctr;
        }
      }
      ctr = ctr + 1;
    }
    pivots.toList
  }

  /**
  * From some pivot point, calculate all words that can be played as you move
  * left across blanks until you reach the stopping point.
  *
  * @param board         The board to analysize
  * @param rowNum        The row to examine
  * @param pivotStart    The index of the first char in a pivot
  * @param stoppingPoint The stopping point
  * @return              words
  */
  def goLeft(
    board: Board
    , rowNum: Int
    , letters: List[Char]
    , combos: List[List[Char]]
    , pivotStart: Int
    , stoppingIndex: Int
  ) : List[Play] = {

    val row = board.rows(rowNum);

    def goLeft_h(
      letters: List[Char]
      , index: Int
      , numBlanks: Int
    ): List[Play] = {
      val numLetters = letters.size;
      if (index < stoppingIndex || (numBlanks > numLetters)) {
        List()
      } else if (index > 0 && row(index - 1) != ' ') {
        // We want to stop here, because it means this blank is adjacent to a
        // previous pivot.  This situation is covered by going right from the
        // previous pivot, which is something we do.
        List()
        //goRight(board, rowNum, letters, combos, index + 1);
      } else {
        val plays = combos.map {l => (l, board.across(l.mkString, rowNum, index))}

        val goodPlays = plays.filter {p => p._2.isInstanceOf[GoodPlay]}


        val withIdx = goodPlays.map {p => Play(p._1, rowNum, index, p._2)}

        withIdx ++ goLeft_h(letters, index - 1, numBlanks + 1)
      }
    }
    goLeft_h(letters, pivotStart-1, 1)//, List());
  }

  // From some point on the board, go right forming words given the letters in
  // your hand.
  def goRight(
    board: Board
    , rowNum: Int
    , letters: List[Char]
    , combos: List[List[Char]]
    , startingPoint: Int
  ): List[Play] = {

    val row = board.rows(rowNum);

    val plays = combos.map {l => 
      (l, board.across(l.mkString, rowNum, startingPoint))
    }

    val goodPlays = plays.filter {p => p._2.isInstanceOf[GoodPlay]}

    goodPlays.map {p => Play(p._1, rowNum, startingPoint, p._2)}
  }
}

class Combinator {

  def computeCombos(letters: List[Char]): List[List[Char]] = {
    
    def stripper(lst: List[List[Char]]): List[List[Char]] = {
      if (lst.isEmpty || lst(0).size == 2)
        List()
      else {
        val tmp = lst.map {_.drop(1)}
        tmp ++ stripper(tmp)
      }
    }

    val lettersHandled = new scala.collection.mutable.HashMap[List[Char], List[List[Char]]]();

    def combinations(letters: List[Char]): List[List[Char]] = {
      def removeFirst(l: Char): List[Char] = 
        letters.indexOf(l) match {
        case -1 => letters
        case i => letters.take(i) ++ letters.drop(i + 1)
      }


      if (letters.size == 0)
        List()
      else if (letters.size == 1)
        List(List(letters(0)))
      else { 
        letters.flatMap {c => 
        /*
          if (lettersHandled.contains(letters))
            lettersHandled(letters)
          else {
            val tmp = combinations(removeFirst(c)).map {r => c :: r}
            lettersHandled + (letters -> tmp)
            tmp
          }
          */
          combinations(removeFirst(c)).map {r => c :: r}
        }.removeDuplicates
      }
    }

    val tmp = combinations(letters);
    tmp ++ stripper(tmp).removeDuplicates ++ letters.removeDuplicates.map {List(_)}
  }
}
