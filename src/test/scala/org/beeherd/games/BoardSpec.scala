package org.beeherd.games;

import org.specs._;
import org.specs.runner.JUnit4;


class BoardSpecTest extends JUnit4(BoardSpec)
object BoardSpec extends Specification {

  "The Board" should {
    val start = Game.createBoard(15);

    "guarantee that the first play on the board crosses its center point" in {
      val letters = "hi"
      start.down(letters, 2, 2) match {
        case BadPlay(_,_) => {}
        case _ => fail("Expected BadPlay")
      }
    }

    "allow a player to create the first word if it crosses the center point" in {
      val letters = "hi"
      val newBoard = start.down(letters, 7,7).board;
      val words = newBoard.createdWords(letters,7,7);
      words must haveSize(1);
      words must contain("hi");
    }

    "allow a player to add characters to the bottom of a word to form a new word" in {
      val b = start.down("hi", 7,7).board;
      val letters = "s";
      val r = b.down(letters, 9, 7);
      val b2 = r.board;
      val words = b2.createdWords(letters,9,7);
      b must_!= b2
      words must haveSize(1);
      words must contain("his");
    }

    "allow a player to play across" in {
    }

    "not allow a word to go off the board" in {
      val b = start.down("hi", 7, 7).board;
      val letters = "sssssssssssssssssssss";
      val r = b.down(letters, 9,7);
      r match {
        case BadPlay(b2, msg) => {
          b2 must_== b;
          msg must include("ss does not fit on the board.");
        }
        case _ => fail("Expected BadPlay")
      }
    }

    "not allow a player to remove characters from the board" in {
      val b = start.down("hi", 7,7).board;
      val letters = "o";
      b.down(letters, 8,7) match {
        case BadPlay(b2, msg) => {
          b2 must_== b;
          msg must include("A character already exists on the starting point.");
        }
        case _ => fail("Expected BadPlay")
      }
    }

    "not allow non-alphabet characters on the board" in {
      val b = start.down("hi", 7,7).board;
      val letters = "0";
      b.down(letters, 9,7) match {
        case BadPlay(b2, msg) => {
          b2 must_== b;
          msg must include("must be letters.");
        }
        case _ => fail("Expected BadPlay")
      }
    }

    "allow a player to add characters that touch multiple existing plays" in {
    }

    "handle a bunch of cases that have caused problems" in {
      /* Dictionary.isWord prevents testing this
      val b = start.down("hi", 2, 2)._1;
      val letters = "h";
      val r = b.across(letters, 2, 3);
      r._1.createdWords("h",2,3) must haveSize(1);
      r._1.createdWords("h",2,3) must contain("hh");
      */
    }
  }
}
