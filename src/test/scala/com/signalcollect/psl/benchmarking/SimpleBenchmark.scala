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

package com.signalcollect.psl.benchmarking

import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig

object SimpleBenchmark extends App {

  val movieExample1 = """
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

  //  val movieExample1 = """
  //    	predicate: likes(_, _)
  //    	    
  //    	rule [weight = 1]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
  //     	
  //    	fact [truthValue = 1.0]: likes(sara, pulp-fiction)
  //    fact [truthValue = 1.0]: likes(sara, blade-runner)
  //    	fact [truthValue = 1.0]: likes(philip, pulp-fiction)
  //    	"""

  val movieExample2 = """
  	predicate: likes(_, _)
  	predicate: playsIn(_, _)
  	    
  	rule [weight = 1, distanceMeasure = experimentalSquared]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
  	rule [weight = 1, distanceMeasure = experimentalSquared]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
  	rule [weight = 1, distanceMeasure = experimentalSquared]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
  
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

  //  val movieExample2 = """
  //    	predicate: likes(_, _)
  //    	    
  //    	rule [weight = 1000, distanceMeasure = experimentalSquared]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
  //     	
  //    	fact [truthValue = 1.0]: likes(sara, pulp-fiction)
  //    fact [truthValue = 1.0]: likes(sara, blade-runner)
  //    	fact [truthValue = 1.0]: likes(philip, pulp-fiction)
  //    	"""

//  val movieExample3 = """
//class Person: sara, philip, fred, george, tom-cruise, james-stewart, harrison-ford
//class Movie: mission-impossible, rear-window, vertigo, top-gun, blade-runner, indiana-jones-and-the-last-crusade, star-wars
//class Actor: james-stewart, tom-cruise, harrison-ford
//
//predicate: likes(Person, _)
//predicate: playsIn(Actor, Movie)
//
//rule [weight = 5]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
//rule [weight = 5]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
//rule [weight = 1]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)
//  
//fact: playsIn(tom-cruise, top-gun)
//fact: playsIn(tom-cruise, mission-impossible)
//fact: playsIn(james-stewart, rear-window)
//fact: playsIn(james-stewart, vertigo)
//fact: playsIn(harrison-ford, indiana-jones-and-the-last-crusade)
//fact: playsIn(harrison-ford, blade-runner)
//fact: playsIn(harrison-ford, star-wars)
//
//fact [truthValue = 0.1]: likes(sara, tom-cruise)
//fact [truthValue = 0.9]: likes(sara, harrison-ford)
//
//fact [truthValue = 0.9]: likes(sara, pulp-fiction)
//fact [truthValue = 0.8]: likes(sara, star-wars)
//fact [truthValue = 0.8]: likes(sara, blade-runner)
//
//fact [truthValue = 0.9]: likes(philip, pulp-fiction)
//fact [truthValue = 1.0]: likes(philip, blade-runner)
//fact [truthValue = 0.8]: likes(philip, harrison-ford)
//fact [truthValue = 1.0]: likes(philip, rear-window)
//fact [truthValue = 0.2]: likes(philip, top-gun)
//
//fact [truthValue = 1.0]: likes(fred, star-wars)
//"""

    val movieExample4 = """
class Person: sara, philip, fred, george, tim, tom-cruise, james-stewart, harrison-ford
class Movie: mission-impossible, rear-window, vertigo, top-gun, blade-runner, indiana-jones-and-the-last-crusade, star-wars, grease
class Actor: james-stewart, tom-cruise, harrison-ford

predicate: likes(Person, _)
predicate: playsIn(Actor, Movie)

rule [weight = 8]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-A) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
rule [weight = 4]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
rule [weight = 2]: likes(PERSON-A, A) && likes(PERSON-B, A) && likes(PERSON-B, B) => likes(PERSON-A, B)

fact: playsIn(tom-cruise, top-gun)
fact: playsIn(tom-cruise, mission-impossible)
fact: playsIn(james-stewart, rear-window)
fact: playsIn(james-stewart, vertigo)
fact: playsIn(harrison-ford, indiana-jones-and-the-last-crusade)
fact: playsIn(harrison-ford, blade-runner)
fact: playsIn(harrison-ford, star-wars)

fact [truthValue = 0.1]: likes(sara, tom-cruise)
fact [truthValue = 0.9]: likes(sara, harrison-ford)
fact [truthValue = 0.9]: likes(sara, pulp-fiction)
fact [truthValue = 0.8]: likes(sara, star-wars)
fact [truthValue = 0.8]: likes(sara, blade-runner)

fact [truthValue = 0.9]: likes(philip, pulp-fiction)
fact [truthValue = 1.0]: likes(philip, blade-runner)
fact [truthValue = 1.0]: likes(philip, rear-window)
fact [truthValue = 0.2]: likes(philip, top-gun)

fact [truthValue = 1.0]: likes(george, harrison-ford)
fact [truthValue = 1.0]: likes(george, grease)

fact [truthValue = 1.0]: likes(fred, star-wars)
"""
  
  val startTime = System.currentTimeMillis
  val config = InferencerConfig(
    globalConvergenceDetection = Some(128),
    //    globalConvergenceDetection = None,
    maxIterations = 10000,
    serializeMessages = false,
    absoluteEpsilon = 1e-8,
    relativeEpsilon = 1e-8,
    isBounded = true,
    objectiveLoggingEnabled = true,
    eagerSignalCollectConvergenceDetection = true,
    stepSize = 1.0)
  val inferenceResults = Inferencer.runInferenceFromString(
    movieExample4, config = config)
  val durationInSeconds = ((System.currentTimeMillis - startTime) / 100.0).round / 10.0
  println(s"Running inference took $durationInSeconds seconds.")
  println(inferenceResults)
  //println(s"dual residual sum = ${inferenceResults.solution.convergence.get.dualResidualForSteps.values.sum}")
  //println("\n" + ConvergencePlotter.createPlotScript(inferenceResults.solution.convergence) + "\n")
}
