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

  /**
   * Calling SDPT3 4.0: 628 variables, 294 equality constraints
   * For improved efficiency, SDPT3 is solving the dual problem.
   * ------------------------------------------------------------
   * num. of constraints = 294
   * dim. of sdp    var  = 240,   num. of sdp  blk  = 120
   * dim. of linear var  = 268
   * 8 linear variables from unrestricted variable.
   * ** convert ublk to lblk
   * ******************************************************************
   * SDPT3: Infeasible path-following algorithms
   * ******************************************************************
   * version  predcorr  gam  expon  scale_data
   * HKM      1      0.000   1        0
   * it pstep dstep pinfeas dinfeas  gap      prim-obj      dual-obj    cputime
   * -------------------------------------------------------------------
   * 0|0.000|0.000|4.5e+01|1.2e+01|1.6e+05| 3.622864e+03  0.000000e+00| 0:0:00| chol  1  1
   * 1|0.770|0.486|1.0e+01|6.2e+00|8.7e+04| 5.168828e+03 -1.425625e+03| 0:0:00| chol  1  1
   * 2|1.000|0.861|8.2e-06|9.3e-01|2.2e+04| 6.749780e+03 -4.264488e+03| 0:0:00| chol  1  1
   * 3|1.000|0.951|2.3e-06|6.7e-02|4.6e+03| 3.037872e+03 -1.155609e+03| 0:0:00| chol  1  1
   * 4|1.000|0.924|6.1e-07|7.3e-03|7.6e+02| 5.152219e+02 -2.309138e+02| 0:0:00| chol  1  1
   * 5|0.971|0.766|5.4e-07|1.9e-03|1.3e+02| 7.202479e+01 -5.534232e+01| 0:0:00| chol  1  1
   * 6|1.000|0.677|4.4e-07|6.3e-04|6.1e+01| 3.504523e+01 -2.551131e+01| 0:0:00| chol  1  1
   * 7|0.696|0.469|2.1e-07|3.4e-04|4.0e+01| 2.329712e+01 -1.646573e+01| 0:0:01| chol  1  1
   * 8|0.982|0.680|8.3e-08|1.1e-04|1.2e+01| 5.189487e+00 -6.370097e+00| 0:0:01| chol  1  1
   * 9|1.000|0.291|3.0e-08|7.6e-05|8.4e+00| 3.481246e+00 -4.853634e+00| 0:0:01| chol  1  1
   * 10|1.000|0.671|2.0e-08|2.5e-05|3.1e+00| 1.176598e+00 -1.855712e+00| 0:0:01| chol  1  1
   * 11|1.000|0.683|5.1e-09|8.0e-06|8.2e-01| 1.802779e-01 -6.265516e-01| 0:0:01| chol  1  1
   * 12|1.000|0.890|9.6e-10|8.7e-07|9.8e-02| 2.248073e-02 -7.439694e-02| 0:0:01| chol  1  1
   * 13|1.000|0.680|1.2e-10|2.8e-07|3.1e-02| 5.310724e-03 -2.485759e-02| 0:0:01| chol  1  1
   * 14|1.000|0.791|6.5e-11|7.2e-07|7.7e-03| 1.793802e-03 -5.622979e-03| 0:0:01| chol  1  1
   * 15|0.986|0.927|9.6e-12|6.0e-07|7.6e-04| 2.376834e-04 -4.630373e-04| 0:0:01| chol  1  1
   * 16|1.000|0.892|3.0e-11|5.8e-08|3.2e-04| 2.080348e-04 -1.102509e-04| 0:0:01| chol  1  1
   * 17|1.000|0.954|5.7e-13|2.5e-08|5.8e-05| 3.895091e-05 -1.732072e-05| 0:0:01| chol  1  1
   * 18|1.000|0.926|2.6e-13|4.5e-09|1.2e-05| 8.120723e-06 -3.744725e-06| 0:0:01| chol  1  1
   * 19|1.000|0.941|9.7e-14|9.4e-10|2.3e-06| 1.569944e-06 -7.025617e-07| 0:0:01| chol  1  1
   * 20|1.000|0.936|4.4e-14|1.8e-10|4.8e-07| 3.205879e-07 -1.429199e-07| 0:0:01| chol  1  1
   * 21|1.000|0.939|2.4e-13|3.7e-11|1.0e-07| 6.567902e-08 -2.883599e-08| 0:0:01| chol  1  1
   * 22|1.000|0.948|2.3e-12|7.8e-12|2.2e-08| 1.367431e-08 -5.734770e-09| 0:0:01| chol  1  1
   * 23|1.000|0.962|8.9e-13|1.7e-12|4.7e-09| 2.957203e-09 -1.151388e-09| 0:0:01|
   * stop: max(relative gap, infeasibilities) < 1.49e-08
   * -------------------------------------------------------------------
   * number of iterations   = 23
   * primal objective value =  2.95720275e-09
   * dual   objective value = -1.15138828e-09
   * gap := trace(XZ)       = 4.67e-09
   * relative gap           = 4.67e-09
   * actual relative gap    = 4.11e-09
   * rel. primal infeas     = 8.90e-13
   * rel. dual   infeas     = 1.72e-12
   * norm(X), norm(y), norm(Z) = 1.1e+03, 1.3e+01, 1.8e+01
   * norm(A), norm(b), norm(C) = 3.7e+01, 1.2e+01, 2.5e+01
   * Total CPU time (secs)  = 1.21
   * CPU time per iteration = 0.05
   * termination code       =  0
   * DIMACS: 5.3e-12  0.0e+00  1.5e-11  0.0e+00  4.1e-09  4.7e-09
   * -------------------------------------------------------------------
   * ------------------------------------------------------------
   * Status: Solved
   * Optimal value (cvx_optval): +1.15139e-09
   */

  var results = "["
  results += ExperimentHelper.run(votingExample, 3, 5)
  results += "]"
  //println(results.toString)

  /**
   *
   * [0.001 0.001 0 0 1.0 40 7540 7.001209575887982E-4
   * 0.001 1.0E-4 0 0 1.0 42 7476 4.453360618320156E-4
   * 1.0E-4 0.001 0 0 1.0 56 9979 1.6024731458052962E-5
   * 1.0E-4 1.0E-4 0 0 1.0 62 10858 3.5442815087442357E-6
   * 0.001 0.001 0 1 1.0 56 9900 5.590075575019346E-4
   * 0.001 1.0E-4 0 1 1.0 56 10023 5.590075575019346E-4
   * 1.0E-4 0.001 0 1 1.0 78 14175 2.8524929709940265E-6
   * 1.0E-4 1.0E-4 0 1 1.0 78 13353 2.8524929709940265E-6
   * 0.001 0.001 1 0 1.0 40 6833 7.482886809264725E-5
   * 0.001 1.0E-4 1 0 1.0 40 7225 7.482886809264725E-5
   * 1.0E-4 0.001 1 0 1.0 52 9039 6.740603724127946E-8
   * 1.0E-4 1.0E-4 1 0 1.0 56 10128 3.51565579832333E-8
   * 0.001 0.001 1 1 1.0 54 9516 1.3411755357825835E-4
   * 0.001 1.0E-4 1 1 1.0 54 9495 1.3411755357825835E-4
   * 1.0E-4 0.001 1 1 1.0 70 12926 2.033630920809925E-9
   * 1.0E-4 1.0E-4 1 1 1.0 70 12620 2.033630920809925E-9
   * ]
   */
}

object ExperimentHelper {
  def run(pslString: String, minExponent: Int = 5, maxExponent: Int = 8): String = {
    var results = ""
    val stepIncrement = 10
    val pslData = PslParser.parse(pslString)

    for { i <- 1 until stepIncrement } {
      results += experiment(pslData, minExponent, maxExponent, i.toDouble / stepIncrement)
    }
    results
  }

  def experiment(pslData: ParsedPslFile, minExponent: Int, maxExponent: Int, stepSize: Double = 1.0): String = {
    val diff = maxExponent - minExponent
    var results = ""

    for { i <- minExponent until maxExponent } {
      for { j <- minExponent until maxExponent } {
        val config = new InferencerConfig(
          computeObjectiveValueOfSolution = true,
          absoluteEpsilon = Math.pow(10, -i),
          relativeEpsilon = Math.pow(10, -j),
          isBounded = false,
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
          isBounded = true,
          stepSize = stepSize)
        results += runExperiment(pslData, 2 * diff * diff + i * diff + j, config)
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

    for { i <- minExponent until maxExponent } {
      for { j <- minExponent until maxExponent } {
        val config = new InferencerConfig(
          computeObjectiveValueOfSolution = true,
          absoluteEpsilon = Math.pow(10, -i),
          relativeEpsilon = Math.pow(10, -j),
          isBounded = true,
          stepSize = stepSize)
        results += runExperiment(pslData, 6 * diff * diff + i * diff + j, config)
      }
    }

    results
  }

  def runExperiment(pslData: ParsedPslFile, id: Int, config: InferencerConfig) = {
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    val execStats = inferenceResults.solution.stats.executionStatistics
    val signalSteps = execStats.signalSteps
    val computationTime = execStats.computationTime.toUnit(TimeUnit.MILLISECONDS).toInt
    val vertices = inferenceResults.solution.stats.aggregatedWorkerStatistics.numberOfVertices
    val edges = inferenceResults.solution.stats.aggregatedWorkerStatistics.numberOfOutgoingEdges
    val objDiff = inferenceResults.objectiveFun.get
    s"$config.absoluteEpsilon $config.relativeEpsilon ${if (config.isBounded) 1 else 0} 0 $config.stepSize $signalSteps $computationTime $objDiff \n"
  }
}
