/*
 *  @author Philip Stutz
 *  @author Sara Magliacane
 *
 *  Copyright 2014 University of Zurich & VU University Amsterdam
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */


package com.signalcollect.psl.examples

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

import com.signalcollect.admm.Wolf
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.Grounding
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.PSLToCvxConverter
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile

import scala.annotation.tailrec

/**
 * Small example that plays around with default values and contrasting rules.
 */
class FriendsExample extends FlatSpec with Matchers with TestAnnouncements {

  val friends = """
	predicate: 		friends(_, _)
    predicate: True()
    
    // Gives a default soft truth value of 1/(4+1) to unknown predicates.
    rule [1]: True() => friends(A,B)
    rule [4]: True() => !friends(A,B)
    
    fact [1.0]: True()
	fact: friends(anna, bob)
    fact: !friends(bob, carl)
	"""
  "FriendsExample" should "provide a solution consistent for friends, with a default value of 0.2" in {
    val pslData = PslParser.parse(friends)

    val config = InferencerConfig(objectiveLoggingEnabled = true, absoluteEpsilon = 1e-08, relativeEpsilon = 1e-03, isBounded = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(3.2 +- 6e-2)
  }

  /**
   * Not bounded
   * --------------
   * - Execution Statistics -
   * --------------
   * # signal steps		38
   * # collect steps		38
   * Computation time		6711 milliseconds
   * Master JVM CPU time	4511 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	1577
   * # signal operations	1577
   * # vertices (add/remove)	83 (83/0)
   * # edges (add/remove)	204 (204/0)
   *
   * GroundedPredicate 1: friends[ ] (carl, anna) has truth value 0.20270518891142217
   * GroundedPredicate 2: friends[ ] (anna, carl) has truth value 0.20270518891142228
   * GroundedPredicate 11: friends[ ] (bob, anna) has truth value 0.20270518891142217
   * GroundedPredicate 4: friends[ ] (carl, bob) has truth value 0.20270518891142233
   * Objective function value: 3.2001463609409293
   *
   * Bounded
   * --------------
   * - Execution Statistics -
   * --------------
   * # signal steps		40
   * # collect steps		40
   * Computation time		7089 milliseconds
   * Master JVM CPU time	3765 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	1140
   * # signal operations	1140
   * # vertices (add/remove)	57 (57/0)
   * # edges (add/remove)	152 (152/0)
   *
   * GroundedPredicate 1: friends[ ] (carl, anna) has truth value 0.20105260181969786
   * GroundedPredicate 2: friends[ ] (anna, carl) has truth value 0.2010526018196978
   * GroundedPredicate 11: friends[ ] (bob, anna) has truth value 0.2010526018196978
   * GroundedPredicate 4: friends[ ] (carl, bob) has truth value 0.2010526018196978
   * Objective function value: 3.200022159411816
   */

  val freenemies = """
	predicate: friends(_, _)
    predicate: True()
    
    // negative weights make a convex problem concave... not converging.
    // is this case it's actually a linear problem, so it works.
    rule [weight = -1, distanceMeasure = linear]: True() => friends(A,B)
    
    fact: True()
	fact: friends(anna, bob)
    fact: !friends(bob, carl)
	"""
  "FriendsExample" should "provide a solution consistent for freenemies, an example with negative weights" in {
    val pslData = PslParser.parse(freenemies)

    val config = InferencerConfig(objectiveLoggingEnabled = true, absoluteEpsilon = 10e-08, relativeEpsilon = 10e-03, isBounded = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(0.0 +- 6e-2)
  }

  // No friends except the explicitly mentioned (as std in CWA).
  val enemies = """
	predicate: 		friends(_, _)
    predicate: True()
    
    rule [1]: True() => !friends(A,B)
    
    fact: True()
	fact: friends(anna, bob)
    fact: !friends(bob, carl)
	"""
  "FriendsExample" should "provide a solution consistent for enemies, an example with negative prior" in {
    val pslData = PslParser.parse(enemies)

    val config = InferencerConfig(objectiveLoggingEnabled = true, absoluteEpsilon = 10e-08, relativeEpsilon = 10e-03, isBounded = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(0.0 +- 6e-2)
  }

  // No friends except the explicitly mentioned (as std in CWA).
  val hardenemies = """
	predicate: 		friends(_, _)
    predicate: True()
    
    rule[1]: True() => friends(A,B)
    rule[1]: True() => !friends(A,B)
    
    rule: friends(A,B) => friends(B,A)
    rule: !friends(A,B) => !friends(B,A)
    
    fact: True()
	fact: friends(anna, bob)
    fact: !friends(bob, carl)
	"""
 
}
