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

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    //println(inferenceResults)
    //println("Objective function value: " + objectiveFunctionVal)

    objectiveFunctionVal should be(0.02 +- 0.02)

    //println(PSLToCvxConverter.toCvx(simplifiedMovieExample)) 
    //println("\n" + ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }

  /**
   * simplifiedMovieExample: ~30 seconds in matlab
   *
   * 02/04/2014 @ 15:35
   *
   * ------------------------
   * - Execution Parameters -
   * ------------------------
   * Execution mode 		Synchronous
   * Signal threshold 	0.01
   * Collect threshold 	0.0
   * Time limit 		None
   * Steps limit 		None
   *
   * --------------
   * - Execution Statistics -
   * --------------
   * # signal steps		104
   * # collect steps		104
   * Computation time		18662 milliseconds
   * Master JVM CPU time	7488 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	43524
   * # signal operations	43524
   * # vertices (add/remove)	837 (837/0)
   * # edges (add/remove)	4620 (4620/0)
   *
   * Problem solved, getting back results.
   * GroundedPredicate(16,relation: likes(_, _),List(sara, grease),None) has truth value 0.7889580257363453
   * GroundedPredicate(15,relation: likes(_, _),List(philip, star-wars),None) has truth value 0.4781222657422232
   * Objective function value: 0.0352199576947269
   *
   *
   * Matlab ~ 1 minute
   *
   * Calling SDPT3 4.0: 3424 variables, 1712 equality constraints
   * ------------------------------------------------------------
   *
   * num. of constraints = 1712
   * dim. of sdp    var  = 1660,   num. of sdp  blk  = 830
   * dim. of linear var  = 934
   * ******************************************************************
   * SDPT3: Infeasible path-following algorithms
   * ******************************************************************
   * version  predcorr  gam  expon  scale_data
   * HKM      1      0.000   1        0
   * it pstep dstep pinfeas dinfeas  gap      prim-obj      dual-obj    cputime
   * -------------------------------------------------------------------
   * 0|0.000|0.000|8.0e+01|3.4e+01|1.9e+06| 8.300000e+03  0.000000e+00| 0:0:00| spchol  1  1
   * 1|0.577|0.653|3.4e+01|1.2e+01|9.8e+05| 2.774122e+04 -3.050811e+04| 0:0:00| spchol  1  1
   * 2|0.605|0.789|1.3e+01|2.6e+00|6.1e+05| 5.827347e+04 -8.121006e+04| 0:0:00| spchol  1  1
   * 3|0.936|1.000|8.5e-01|2.7e-02|1.5e+05| 5.637226e+04 -7.086292e+04| 0:0:01| spchol  1  1
   * 4|0.907|0.913|7.9e-02|9.8e-03|2.1e+04| 6.018236e+03 -1.385711e+04| 0:0:01| spchol  1  1
   * 5|1.000|1.000|2.1e-08|1.7e-02|9.1e+03| 3.865070e+03 -5.185175e+03| 0:0:01| spchol  1  1
   * 6|0.952|0.914|3.2e-09|1.5e-03|1.1e+03| 2.686515e+02 -8.198909e+02| 0:0:01| spchol  1  1
   * 7|1.000|1.000|5.0e-10|8.1e-06|4.7e+02| 1.660420e+02 -3.055781e+02| 0:0:01| spchol  1  1
   * 8|0.912|0.890|2.0e-10|1.6e-06|9.7e+01| 3.078488e+01 -6.571420e+01| 0:0:02| spchol  1  1
   * 9|1.000|1.000|3.7e-11|8.1e-08|5.1e+01| 1.676693e+01 -3.410164e+01| 0:0:02| spchol  1  1
   * 10|0.943|0.990|2.1e-12|8.9e-09|1.0e+01| 3.764464e+00 -6.460864e+00| 0:0:02| spchol  1  1
   * 11|1.000|1.000|3.2e-16|8.1e-10|5.5e+00| 1.794467e+00 -3.684565e+00| 0:0:02| spchol  1  1
   * 12|0.955|0.952|1.6e-15|1.2e-10|3.9e-01| 1.438986e-01 -2.509707e-01| 0:0:02| spchol  1  1
   * 13|1.000|0.964|1.7e-15|1.3e-11|5.3e-02| 3.444967e-02 -1.903265e-02| 0:0:02| spchol  1  1
   * 14|1.000|1.000|4.7e-15|1.8e-12|2.4e-02| 2.762450e-02  3.901834e-03| 0:0:03| spchol  1  1
   * 15|0.920|0.920|3.0e-15|1.2e-12|2.2e-03| 2.069691e-02  1.852745e-02| 0:0:03| spchol  1  1
   * 16|1.000|1.000|2.3e-14|1.0e-12|9.0e-04| 2.028874e-02  1.938988e-02| 0:0:03| spchol  1  1
   * 17|1.000|1.000|3.4e-14|1.0e-12|1.3e-04| 2.004251e-02  1.991022e-02| 0:0:03| spchol  1  1
   * 18|1.000|1.000|3.5e-13|1.0e-12|2.6e-05| 2.000826e-02  1.998255e-02| 0:0:03| spchol  1  1
   * 19|1.000|1.000|3.9e-12|1.0e-12|4.9e-06| 2.000158e-02  1.999667e-02| 0:0:04| spchol  1  1
   * 20|1.000|1.000|5.6e-12|1.0e-12|9.3e-07| 2.000030e-02  1.999937e-02| 0:0:04| spchol  1  1
   * 21|1.000|1.000|5.6e-11|1.1e-12|1.8e-07| 2.000006e-02  1.999988e-02| 0:0:04| spchol  2  2
   * 22|1.000|1.000|3.2e-12|1.7e-12|3.6e-08| 2.000001e-02  1.999998e-02| 0:0:04| spchol  2  2
   * 23|1.000|1.000|7.3e-12|5.3e-13|7.3e-09| 2.000000e-02  2.000000e-02| 0:0:04|
   * stop: max(relative gap, infeasibilities) < 1.49e-08
   * -------------------------------------------------------------------
   * number of iterations   = 23
   * primal objective value =  2.00000021e-02
   * dual   objective value =  1.99999955e-02
   * gap := trace(XZ)       = 7.32e-09
   * relative gap           = 7.04e-09
   * actual relative gap    = 6.40e-09
   * rel. primal infeas     = 7.30e-12
   * rel. dual   infeas     = 5.31e-13
   * norm(X), norm(y), norm(Z) = 5.5e+01, 2.8e-01, 2.9e+01
   * norm(A), norm(b), norm(C) = 7.6e+01, 6.1e+01, 3.0e+01
   * Total CPU time (secs)  = 4.28
   * CPU time per iteration = 0.19
   * termination code       =  0
   * DIMACS: 1.1e-10  0.0e+00  7.9e-12  0.0e+00  6.4e-09  7.0e-09
   * -------------------------------------------------------------------
   * ------------------------------------------------------------
   * Status: Solved
   * Optimal value (cvx_optval): +0.02
   */

}
