/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.beeherd.games

import java.util.Date;

object Test {
  def main(args: Array[String]): Unit = {
    //combos
    play
  }

  def play {
    println("Starting.");
    val start = new Date().getTime;
    val board = Game.createBoard(15, new Dictionary {
        lazy val words = Set("add", "adds", "odd", "addd", "daddy", "ddabb")
        def isWord(w: String) = words.contains(w)
        }
      );
    val result = board.across("dd", 7, 7).asInstanceOf[GoodPlay];
    val a = new BoardAnalyzer();

    val letters = List('a', 'b', 'c', 's', 'd', 'y', 'z');
    //val letters = List('a', 'd', 'y', 'b')
    val combos = new Combinator().computeCombos(letters);

    //combos.filter(_.size == 1).foreach{println _}
    println(combos.size)
    val plays = a.availablePlays(result.board, letters, combos)
    //val downPlays = a.playsDown(result.board, 7,letters)
      
    plays.foreach {p => 
      println(p.row + ":" + p.column + " = "+ p.letters)
      p.result.board.print
    }
    //downPlays.foreach {p => println(p.row + ":" + p.column + " = "+ p.letters)}
    println("Execution time: " + (new Date().getTime - start) / 1000.0 + " seconds.");
  }

  def combos {
    val start = new Date().getTime;
    //val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g');
    val l = List('a', 'b', 'd', 'y')
    //val l = List('a', 'a', 'a')
    val combos = new Combinator().computeCombos(l);
    combos.foreach {println _}
    println("Number of combos:  " + combos.size)
    val end = new Date().getTime;
    println("Exec time: " + ((end - start) / 1000.0))
  }
}
