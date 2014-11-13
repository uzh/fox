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

package com.signalcollect.psl

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.util.TestAnnouncements

class FunctionalSpec extends FlatSpec with Matchers with TestAnnouncements {

  val config = InferencerConfig(computeObjectiveValueOfSolution = true, breezeOptimizer = false, tolerance = 1e-5)

  val one = """
    class A: a
    class C: c1
  	predicate [Functional]: p(A, C)
"""

  "Functional property" should "work correcly with one instance" in {
    val numberOfInstances = 1
    val truthValuePerInstance = 1.0 / numberOfInstances
    val inferenceResults = Inferencer.runInferenceFromString(one, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === truthValuePerInstance +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val two = """
    class A: a
    class C: c1, c2
  	predicate [Functional]: p(A, C)
"""

  "Functional property" should "work correcly with two instances" in {
    val numberOfInstances = 2
    val truthValuePerInstance = 1.0 / numberOfInstances
    val inferenceResults = Inferencer.runInferenceFromString(two, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === truthValuePerInstance +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val twoPlusEvidence = """
    class A: a
    class C: c1, c2
    predicate [Functional]: p(A, C)
    fact [0.1]: p(a, c1)
"""

  "Functional property" should "work correcly with two instances plus evidence" in {
    val inferenceResults = Inferencer.runInferenceFromString(twoPlusEvidence, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === 0.9 +- 1e-5) }
  }

  val three = """
    class A: a
    class C: c1, c2, c3
  	predicate [Functional]: p(A, C)
"""

  "Functional property" should "work correcly with three instances" in {
    val numberOfInstances = 3
    val truthValuePerInstance = 1.0 / numberOfInstances
    val inferenceResults = Inferencer.runInferenceFromString(three, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === truthValuePerInstance +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val threePlusEvidence = """
    class A: a
    class C: c1, c2, c3
    predicate [Functional]: p(A, C)
    fact [0.1]: p(a, c1)
    fact [0.2]: p(a, c2)
"""

  "Functional property" should "work correcly with three instances plus evidence" in {
    val inferenceResults = Inferencer.runInferenceFromString(threePlusEvidence, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === 0.7 +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val ten = """
    class A: a
    class C: c1, c2, c3, c4, c5, c6, c7, c8, c9, c10
  	predicate [Functional]: p(A, C)
"""

  "Functional property" should "work correcly with ten instances" in {
    val numberOfInstances = 10
    val truthValuePerInstance = 1.0 / numberOfInstances
    val inferenceResults = Inferencer.runInferenceFromString(ten, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === truthValuePerInstance +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

  val tenPlusEvidence = """
    class A: a
    class C: c1, c2, c3, c4, c5, c6, c7, c8, c9, c10
    predicate [Functional]: p(A, C)
    fact [0.1]: p(a, c1)
    fact [0.2]: p(a, c2)
"""

  "Functional property" should "work correcly with ten instances plus evidence" in {
    val truthPerRemaining = 0.7 / 8
    val inferenceResults = Inferencer.runInferenceFromString(tenPlusEvidence, config = config)
    inferenceResults.solution.results.foreach { case (id, truthValue) => assert(truthValue === truthPerRemaining +- 1e-5) }
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.0 +- 1e-5))
  }

}
