/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.beeherd.games

trait Dictionary {
  def isWord(w: String): Boolean;
}

class SimpleDictionary extends Dictionary {
  def isWord(w: String): Boolean = w != "hh"
}