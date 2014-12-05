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

import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.util.TestAnnouncements

/**
 * Small example that plays around with default values and contrasting rules.
 */
class FriendsExample extends FlatSpec with Matchers with TestAnnouncements {

  val friends = """
  predicate [prior = 0.2]:    friends(_, _)    
  fact: !friends(bob, carl)
  fact: friends(anna, bob)
  """
  val friendsOriginal = """
  predicate :    friends(_, _)
    
  // Gives a default soft truth value of 1/(4+1) to unknown predicates.
  // 1 + 5*friends(A,B)^2 - 2 friends(A,B) => friends(A,B) = 1/5
  rule [0.01]: friends(A,B)
  rule [0.04]: !friends(A,B)
    
  fact: friends(anna, bob)
  fact: !friends(bob, carl)
  """
  
  "FriendsExample" should "provide a solution consistent for friends, with a default value of 0.2" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(friends, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.032 +- 1e-5))
  }

  val freenemies = """
  predicate: friends(_, _)
  // negative weights make a convex problem concave... not converging.
  // is this case it's actually a linear problem, so it works.
  rule [weight = -1, distanceMeasure = linear]: friends(A,B)
  fact: friends(anna, bob)
  fact: !friends(bob, carl)
  """
  "FriendsExample" should "provide a solution consistent for freenemies, an example with negative weights" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(freenemies, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(-4.0 +- 1e-5))
  }

  // No friends except the explicitly mentioned (as std in CWA).
  val enemies = """
  //predicate [prior = 0.0]:    friends(_, _)
  predicate :    friends(_, _)
  //rule [1]: !friends(A,B)
  fact: friends(anna, bob)
  fact: !friends(bob, carl)
  """
  "FriendsExample" should "provide a solution consistent for enemies, an example with negative prior" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(enemies, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val hardenemies = """
    predicate [Symmetric, prior = 0.5]:    friends(_, _)
    
//    rule[1]: friends(A,B)
//    rule[1]: !friends(A,B)
    
    fact: friends(anna, bob)
    fact: !friends(bob, carl)
  """
  "FriendsExample" should "provide a solution consistent for hardenemies, an example with negative prior and a hard rule" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(hardenemies, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.01 +- 6e-2))
  }
}
