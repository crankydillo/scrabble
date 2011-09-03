/*
 * SpecsRunner.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.games

import org.specs._;
import org.specs.runner.JUnit4;

import org.specs.runner.SpecsFileRunner

object SpecsRunner extends SpecsFileRunner("**/*.scala", ".*")
