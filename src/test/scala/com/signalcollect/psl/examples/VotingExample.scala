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
	  predicate: 	votes(_, _)
	  predicate: 		friends(_, _)
    predicate: 		enemies(_, _)

    rule [weight = 1]: 	votes(A,P) && friends(A,B) => votes(B,P) 
    rule [weight = 1]: 	votes(A,P) && enemies(A,B) => !votes(B,P) 
    rule: 	enemies(A,B) => !friends(A,B) 
    rule: 	friends(A,B) => !enemies(A,B)  
    
	  fact: friends(anna, bob)
    fact: friends(bob, anna)
	  fact [truthValue = 0.8]: votes(anna, democrats)
    fact [truthValue = 0.2]: votes(carl, repub)
    fact: enemies(carl, bob)
    fact: enemies(bob, carl)
	"""
  "VotingExample" should "provide a solution consistent with Matlab" in {
    val pslData = PslParser.parse(votingExample)

    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 5e-5))
  }
}
