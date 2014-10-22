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

class MovieRecommendationsWithClasses extends FlatSpec with Matchers with TestAnnouncements {

  val movieExampleWithClasses = """
    class Movie: pulp-fiction, trainspotting, mission-impossible, blade-runner, star-wars, grease
    class Person: sara, philip, fred
	class Actor: john-travolta, tom-cruise, ewan-mcgregor, samuel-l-jackson
    
	predicate: likes(Person, _)
	predicate: playsIn(Actor, Movie)
#	predicate: likes(_, _)
#	predicate: playsIn(_, _)
	    
	rule [weight = 1]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
	rule [weight = 1]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-B) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
	rule [weight = 1]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)

	fact: playsIn(john-travolta, pulp-fiction)
	fact: playsIn(samuel-l-jackson, pulp-fiction)
	fact: playsIn(ewan-mcgregor, trainspotting)
	fact: playsIn(tom-cruise, mission-impossible)
	
	fact [truthValue = 0.1]: likes(sara, tom-cruise)
	fact [truthValue = 0.5]: likes(sara, john-travolta)
	fact [truthValue = 0.9]: likes(sara, samuel-l-jackson)
	fact [truthValue = 1.0]: likes(sara, ewan-mcgregor)
	
	fact [truthValue = 0.9]: likes(sara, pulp-fiction)
	fact [truthValue = 0.3]: likes(sara, grease)
	fact [truthValue = 0.8]: likes(sara, star-wars)
	fact [truthValue = 0.8]: likes(sara, transpotting)
	fact [truthValue = 0.8]: likes(sara, blade-runner)
	
	fact [truthValue = 0.9]: likes(philip, pulp-fiction)
	fact [truthValue = 0.8]: likes(philip, john-travolta)
	fact [truthValue = 1.0]: likes(philip, blade-runner)
	
	fact [truthValue = 1.0]: likes(fred, trainspotting) 
	"""

  "MovieRecommendationsWithClasses" should "correctly minimize the simplified movie example" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 10e-08, relativeEpsilon = 10e-03, isBounded = true,
        removeSymmetricConstraints = false)
    val inferenceResults = Inferencer.runInferenceFromString(movieExampleWithClasses, config = config)

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    //println(inferenceResults)
    //println("Objective function value: " + objectiveFunctionVal)

    //println(PSLToCvxConverter.toCvx(simplifiedMovieExample)) 
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }
    
}
