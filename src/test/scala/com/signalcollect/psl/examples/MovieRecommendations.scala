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
import com.signalcollect.util.TestAnnouncements

class MovieRecommendations extends FlatSpec with Matchers with TestAnnouncements {

  val movieExample = """
	predicate: likes(_, _)
	predicate: playsIn(_, _)
	    
	rule [weight = 1]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
	rule [weight = 1]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
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
  val simplifiedMovieExample = """
	predicate: likes(_, _)
	predicate: playsIn(_, _)
	    
	rule [weight = 1]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
	rule [weight = 1]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
	rule [weight = 1]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
	
	fact: playsIn(john-travolta, pulp-fiction)
	fact: playsIn(john-travolta, grease)
	
	fact [truthValue = 0.7]: likes(sara, john-travolta)
	
	fact [truthValue = 0.9]: likes(sara, pulp-fiction)
	fact [truthValue = 0.8]: likes(sara, star-wars)
	
	fact [truthValue = 0.9]: likes(philip, pulp-fiction)
	fact [truthValue = 0.8]: likes(philip, john-travolta)
	fact [truthValue = 0.7]: likes(philip, grease)
	"""

  val simplifiedMovieExampleExperimental = """
	predicate: likes(_, _)
	predicate: playsIn(_, _)
	    
	rule [weight = 0.5, distanceMeasure = experimentalSquared]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
	rule [weight = 0.3, distanceMeasure = experimentalSquared]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
	rule [weight = 0.5, distanceMeasure = experimentalSquared]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
	
	fact: playsIn(john-travolta, pulp-fiction)
	fact: playsIn(john-travolta, grease)
	
	fact [truthValue = 0.7]: likes(sara, john-travolta)
	
	fact [truthValue = 0.9]: likes(sara, pulp-fiction)
	fact [truthValue = 0.8]: likes(sara, star-wars)
	
	fact [truthValue = 0.9]: likes(philip, pulp-fiction)
	fact [truthValue = 0.8]: likes(philip, john-travolta)
	fact [truthValue = 0.7]: likes(philip, grease)
	"""

  "MovieRecommendations" should "correctly minimize the simplified movie example" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(simplifiedMovieExample, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
    objectiveFunctionValOption.foreach(_ should be(0.02 +- 0.02))
  }

}
