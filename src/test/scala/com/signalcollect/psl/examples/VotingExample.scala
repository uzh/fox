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
 * Small example that exploits the functional and symmetric constraints.
 */
class VotingExample extends FlatSpec with Matchers with TestAnnouncements {

  val votingExample = """
	  predicate [Functional]: votes(Person, Party)
	  predicate: 		friends(Person, Person)
    predicate: 		enemies(_, _)

    class Person: anna, bob
    class Party: demo, repub

    rule [weight = 1]: 	votes(A,P) && friends(A,B) => votes(B,P) 
    rule [weight = 1]: 	votes(A,P) && enemies(A,B) => !votes(B,P) 
    rule [2]: 	enemies(A,B) => !friends(A,B) 
    rule [3]: 	friends(A,B) => !enemies(A,B)  
    
	  fact [0.9]: friends(anna, bob)
    fact [0.9]: friends(bob, anna)
	  fact [truthValue = 0.8]: votes(anna, demo)
    fact [truthValue = 0.2]: votes(anna, repub)
    fact [truthValue = 0.2]: votes(carl, repub)
    fact [0.99]: enemies(carl, bob)
    fact [0.99]: enemies(bob, carl)
	"""
  "VotingExample" should "provide a solution consistent with Matlab" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, 
        lazyThreshold = None, absoluteEpsilon = 0, relativeEpsilon = 0)
    val inferenceResults = Inferencer.runInferenceFromString(votingExample, config = config)
    println(inferenceResults)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.24 +- 5e-4))
  }
}
