/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.beeherd.games

import org.specs._;
import org.specs.runner.JUnit4;

class BoardAnalysisSpecTest extends JUnit4(BoardAnalysisSpec)
object BoardAnalysisSpec extends Specification {
  val board = Game.createBoard(15);
  // This is a comment
  "The BoardAnalyzer" should {
    "compute the possible across plays for a row on the board" in {
      val board = Game.createBoard(15, new Dictionary {
          lazy val words = Set("add", "adds", "odd", "daddy")
          def isWord(w: String) = words.contains(w)
        }
      );
      val result = board.across("dd", 7, 7).asInstanceOf[GoodPlay];
      val a = new BoardAnalyzer();

      val plays = a.playsAcross(result.board, 7, List('a', 'b', 'o', 's', 'd', 'y'));
      plays.map {p => (p.letters, p.column)} must haveTheSameElementsAs(List(
        (List('a'), 6)
        , (List('o'), 6)
        , (List('a', 's'), 6)
        , (List('d', 'a', 'y'), 5)
      ));
    }

    "compute the possible down plays for a column on the board" in {
      val board = Game.createBoard(15, new Dictionary {
          lazy val words = Set("add", "adds", "odd", "daddy")
          def isWord(w: String) = words.contains(w)
        }
      );
      val result = board.down("dd", 7, 7).asInstanceOf[GoodPlay];
      val a = new BoardAnalyzer();

      val plays = a.playsDown(result.board, 7, List('a', 'b', 'o', 's', 'd', 'y'));
      plays.map {p => (p.letters, p.row)} must haveTheSameElementsAs(List(
        (List('a'), 6)
        , (List('o'), 6)
        , (List('a', 's'), 6)
        , (List('d', 'a', 'y'), 5)
      ));

    }

    "find all words that can be played to the left of some pivot point when the number of blanks exceeds the number of letters" in {
      val b = board.across("dd", 7, 7);
      val a = new BoardAnalyzer();
      val letters = List('a', 'o');
      val pivot = 7;
      val l = a.goLeft(b.board, 7, letters, pivot, 0);
      l.map {p => (p.letters, p.column)} must haveTheSameElementsAs(List(
         (List('a'), 6)
         , (List('o'), 6)
         , (List('a', 'o'), 6)
         , (List('o', 'a'), 6)
         , (List('a', 'o'), 5)
         , (List('o', 'a'), 5)
      ));
    }

    "find all words that can be played to the left of some pivot point that is bounded by a pivot point to its left" in {
      val b = board.across("dd", 7, 6).board.across("zz", 7, 11);
      val a = new BoardAnalyzer();
      val letters = List('a', 'b', 'c');
      val pivot = 11;
      val l = a.goLeft(b.board, 7, letters, pivot, 0);
      l.map {p => (p.letters, p.column)} must haveTheSameElementsAs(List(
         (List('a'), 10)
         , (List('b'), 10)
         , (List('c'), 10)
         , (List('a', 'b'), 10)
         , (List('a', 'c'), 10)
         , (List('b', 'a'), 10)
         , (List('b', 'c'), 10)
         , (List('c', 'a'), 10)
         , (List('c', 'b'), 10)
         , (List('a', 'b', 'c'), 10)
         , (List('a', 'c', 'b'), 10)
         , (List('b', 'a', 'c'), 10)
         , (List('b', 'c', 'a'), 10)
         , (List('c', 'a', 'b'), 10)
         , (List('c', 'b', 'a'), 10)
         , (List('a', 'b'), 9)
         , (List('a', 'c'), 9)
         , (List('b', 'a'), 9)
         , (List('b', 'c'), 9)
         , (List('c', 'a'), 9)
         , (List('c', 'b'), 9)
         , (List('a', 'b', 'c'), 9)
         , (List('a', 'c', 'b'), 9)
         , (List('b', 'a', 'c'), 9)
         , (List('b', 'c', 'a'), 9)
         , (List('c', 'a', 'b'), 9)
         , (List('c', 'b', 'a'), 9)
         // stops before the blank after the last pivot
      ));
    }

    "find all words going right from some blank and passing over a pivot" in {
      val b = board.across("dd", 7, 7).board;
      val a = new BoardAnalyzer();
      val letters = List('a', 'b', 'c');
      val l = a.goRight(b, 7, letters, 5);
      l.map {p => (p.letters, p.column)} must haveTheSameElementsAs(List(
         (List('a', 'b'), 5)
         , (List('a', 'c'), 5)
         , (List('b', 'a'), 5)
         , (List('b', 'c'), 5)
         , (List('c', 'a'), 5)
         , (List('c', 'b'), 5)
         , (List('a', 'b', 'c'), 5)
         , (List('a', 'c', 'b'), 5)
         , (List('b', 'a', 'c'), 5)
         , (List('b', 'c', 'a'), 5)
         , (List('c', 'a', 'b'), 5)
         , (List('c', 'b', 'a'), 5)
         //, "bbb", "bb"
      ));
    }

    // Going right CAN pass over pivots
    "find all words to the right of some pivot that crosses over another pivot" in {
      val b = board.across("dd", 7, 7).board.across("zz", 7, 11);
      val a = new BoardAnalyzer();
      val letters = List('a', 'b', 'c');
      val l = a.goRight(b.board, 7, letters, 9);
      l.map {p => (p.letters, p.column)} must haveTheSameElementsAs(List(
         (List('a'), 9)
         , (List('b'), 9)
         , (List('c'), 9)
         , (List('a', 'b'), 9)
         , (List('a', 'c'), 9)
         , (List('b', 'a'), 9)
         , (List('b', 'c'), 9)
         , (List('c', 'a'), 9)
         , (List('c', 'b'), 9)
         , (List('a', 'b', 'c'), 9)
         , (List('a', 'c', 'b'), 9)
         , (List('b', 'a', 'c'), 9)
         , (List('b', 'c', 'a'), 9)
         , (List('c', 'a', 'b'), 9)
         , (List('c', 'b', 'a'), 9)
      ));
    }
  }
}


class CombinatorSpecTest extends JUnit4(CombinatorSpec)
object CombinatorSpec extends Specification {
  private val combinator = new Combinator();

  "The Combinator" must {

    "compute combinations of letters" in {
      var letters = List('a','b');
      var l = combinator.computeCombos(letters);
      l must haveTheSameElementsAs(List(
        List('a')
        , List('b')
        , List('a', 'b')
        , List('b', 'a')
      ));
    }
  }
}
