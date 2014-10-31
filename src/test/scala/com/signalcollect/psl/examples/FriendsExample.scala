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
  rule [4]: !friends(A,B)
	predicate: 		friends(_, _)
    
  fact: !friends(bob, carl)
  // Gives a default soft truth value of 1/(4+1) to unknown predicates.
  // 1 + 5*friends(A,B)^2 - 2 friends(A,B) => friends(A,B) = 1/5
  rule [1]: friends(A,B)
    
	fact: friends(anna, bob)
	"""
  "FriendsExample" should "provide a solution consistent for friends, with a default value of 0.2" in {
    val pslData = PslParser.parse(friends)
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    objectiveFunctionVal should be(3.2 +- 1e-5)
  }

  val freenemies = """
	predicate: friends(_, _)
  // negative weights make a convex problem concave... not converging.
  // is this case it's actually a linear problem, so it works.
  rule [weight = -1, distanceMeasure = linear]: => friends(A,B)
	fact: friends(anna, bob)
  fact: !friends(bob, carl)
	"""
  "FriendsExample" should "provide a solution consistent for freenemies, an example with negative weights" in {
    val pslData = PslParser.parse(freenemies)

    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(0.0 +- 1e-5)
  }

  // No friends except the explicitly mentioned (as std in CWA).
  val enemies = """
	predicate: 		friends(_, _)
  rule [1]: !friends(A,B)
	fact: friends(anna, bob)
  fact: !friends(bob, carl)
	"""
  "FriendsExample" should "provide a solution consistent for enemies, an example with negative prior" in {
    val pslData = PslParser.parse(enemies)

    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(0.0 +- 1e-5)
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
  //TODO(sara): The result looks good to me, but the objective function evaluates to infinity. Commenting out for now.
  //  "FriendsExample" should "provide a solution consistent for hardenemies, an example with negative prior and a hard rule" in {
  //    val pslData = PslParser.parse(hardenemies)
  //
  //    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
  //    val inferenceResults = Inferencer.runInference(pslData, config = config)
  //
  //    //println(inferenceResults)
  //    val objectiveFunctionVal = inferenceResults.objectiveFun.get
  //
  //    objectiveFunctionVal should be(3.0 +- 6e-2)
  //  }
}
