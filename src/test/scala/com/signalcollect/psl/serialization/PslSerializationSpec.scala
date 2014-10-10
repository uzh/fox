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

package com.signalcollect.psl.serialization

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

import com.signalcollect.admm.Wolf
import com.signalcollect.admm.utils.ConvergencePlotter
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

class PslSerializationSpec extends FlatSpec with Matchers with TestAnnouncements {

  val example = """
	predicate: likes(_, _)
	predicate: playsIn(_, _)
	    
	rule [weight = 1]: likes(PERSON, MOVIE) && playsIn(ACTOR, MOVIE) => likes(PERSON, ACTOR)
	rule [weight = 1]: likes(PERSON, MOVIE-A) && playsIn(ACTOR, MOVIE-B) && playsIn(ACTOR, MOVIE-B) => likes(PERSON, MOVIE-B)
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

  "PSL Serialization" should "be able to do inference when all messages are serialized" in {
    val config = InferencerConfig(objectiveLoggingEnabled = true, absoluteEpsilon = 10e-09, relativeEpsilon = 10e-04, isBounded = true,
      serializeMessages = true)
    val inferenceResults = Inferencer.runInferenceFromString(example, config = config)

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionVal = inferenceResults.objectiveFun match {
      case Some(v) => v
      case None => Double.MaxValue // will make the test break.
    }

    solution.results.foreach {
      case (id, truthValue) =>
        if (truthValue > 0.01) {
          val gp = gps(id)
          if (!gp.truthValue.isDefined) {
            println(s"$gp has truth value $truthValue")
          }
        }
    }

    println("Objective function value: " + objectiveFunctionVal)

    objectiveFunctionVal should be(0.02 +- 0.02)

    //println(PSLToCvxConverter.toCvx(simplifiedMovieExample)) 
    println("\n" + ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }

}
