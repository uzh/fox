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
import java.util.concurrent.TimeUnit

/**
 * Small example that exploits the functional and symmetric constraints.
 */
class VotingExperiments extends App {

  val votingExample = """
	  predicate: 	votes(_, _)
	  predicate: 		friends(_, _)
    //predicate: 		enemies(_, _)

    rule [weight = 1]: 	votes(A,P) && friends(A,B) => votes(B,P) 
    //rule [weight = 1]: 	votes(A,P) && enemies(A,B) => !votes(B,P) 
    //rule: 	enemies(A,B) => !friends(A,B) 
    //rule: 	friends(A,B) => !enemies(A,B)  
    
	  fact: friends(anna, bob)
    fact: friends(bob, anna)
	  fact [truthValue = 0.8]: votes(anna, democrats)
    fact [truthValue = 0.2]: votes(carl, repub)
    //fact: enemies(carl, bob)
    //fact: enemies(bob, carl)
	"""

  var results = "["
  results += ExperimentHelper.run(votingExample, 3, 5)
  results += "]"
}

object ExperimentHelper {
  def run(pslString: String, minExponent: Int = 5, maxExponent: Int = 8): String = {
    var results = ""
    val stepIncrement = 10

    for { i <- 1 until stepIncrement } {
      results += experiment(pslString, minExponent, maxExponent, i.toDouble / stepIncrement)
    }
    results
  }

  def experiment(pslData: String, minExponent: Int, maxExponent: Int, stepSize: Double = 1.0): String = {
    val diff = maxExponent - minExponent
    var results = ""

    for { i <- minExponent until maxExponent } {
      for { j <- minExponent until maxExponent } {
        val config = new InferencerConfig(
          computeObjectiveValueOfSolution = true,
          absoluteEpsilon = Math.pow(10, -i),
          relativeEpsilon = Math.pow(10, -j),
          isBounded = true,
          stepSize = stepSize)
        results += runExperiment(pslData, i * diff + j, config)
      }
    }

    for { i <- minExponent until maxExponent } {
      for { j <- minExponent until maxExponent } {
        val config = new InferencerConfig(
          computeObjectiveValueOfSolution = true,
          absoluteEpsilon = Math.pow(10, -i),
          relativeEpsilon = Math.pow(10, -j),
          isBounded = false,
          stepSize = stepSize)
        results += runExperiment(pslData, 4 * diff * diff + i * diff + j, config)
      }
    }

    results
  }

  def runExperiment(pslData: String, id: Int, config: InferencerConfig) = {
    val inferenceResults = Inferencer.runInferenceFromString(pslData, config = config)
    val execStats = inferenceResults.solution.stats.get.executionStatistics
    val signalSteps = execStats.signalSteps
    val computationTime = execStats.computationTime.toUnit(TimeUnit.MILLISECONDS).toInt
    val vertices = inferenceResults.solution.stats.get.aggregatedWorkerStatistics.numberOfVertices
    val edges = inferenceResults.solution.stats.get.aggregatedWorkerStatistics.numberOfOutgoingEdges
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
  }
}
