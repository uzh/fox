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
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.PSLToCvxConverter
import com.signalcollect.psl.model.PredicateInRule

import scala.annotation.tailrec

class AnimalClassification extends FlatSpec with Matchers with TestAnnouncements {

  val animalClassExample = """
    class Animal: lisa, blabla
    class AnimalClass: dog, cat, cow
    predicate: barks(Animal)
    predicate: meows(Animal)
    predicate: moohs(Animal)
  	predicate [Functional]: animalClass(Animal, AnimalClass)
  	    
  	rule [weight = 1]: barks(ANIMAL) => animalClass(ANIMAL, dog)
    rule [weight = 1]: meows(ANIMAL) => animalClass(ANIMAL, cat)
    rule [weight = 1]: moohs(ANIMAL) => animalClass(ANIMAL, cow)

    fact [truthValue = 0.1]: barks(lisa)
    fact [truthValue = 0.2]: meows(lisa)
    fact [truthValue = 0.5]: moohs(lisa)
    	"""
  "AnimalClassification" should "provide a solution consistent with Matlab" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, tolerance = 1e-5)  
    val inferenceResults = Inferencer.runInferenceFromString(animalClassExample, config = config)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    objectiveFunctionVal should be(0.0 +- 1e-5)
  }
}
