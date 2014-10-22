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

    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      absoluteEpsilon = 10e-09,
      relativeEpsilon = 10e-04,
      isBounded = false,
      removeSymmetricConstraints = false)
    val inferenceResults = Inferencer.runInference(pslData, config = config)

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    println(inferenceResults)
    println("Objective function value: " + objectiveFunctionVal)

    objectiveFunctionVal should be(0.0 +- 5e-5)

    //println(PSLToCvxConverter.toCvx(votingExample))  
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }
  /**
   * *
   * 02/04/2014 @ 15:55
   * Running inferences for 5 individuals ...
   * Grounding completed: 160 grounded rules, 0 constraints and 60 grounded predicates.
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
   * # signal steps		62
   * # collect steps		62
   * Computation time		11033 milliseconds
   * Master JVM CPU time	4346 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	6634
   * # signal operations	6634
   * # vertices (add/remove)	214 (214/0)
   * # edges (add/remove)	792 (792/0)
   *
   * Problem solved, getting back results.
   * GroundedPredicate(7,relation: votes(_, _),List(bob, democrats),None) has truth value 0.800238673813984
   * Objective function value: 5.696518948162267E-8
   *
   * Calling SDPT3 4.0: 748 variables, 374 equality constraints
   * ------------------------------------------------------------
   *
   * num. of constraints = 374
   * dim. of sdp    var  = 320,   num. of sdp  blk  = 160
   * dim. of linear var  = 268
   * ******************************************************************
   * SDPT3: Infeasible path-following algorithms
   * ******************************************************************
   * version  predcorr  gam  expon  scale_data
   * HKM      1      0.000   1        0
   * it pstep dstep pinfeas dinfeas  gap      prim-obj      dual-obj    cputime
   * -------------------------------------------------------------------
   * 0|0.000|0.000|5.5e+01|2.6e+01|1.8e+05| 1.400000e+03  0.000000e+00| 0:0:00| chol  1  1
   * 1|0.654|0.772|1.9e+01|6.1e+00|7.0e+04| 2.259349e+03 -3.498231e+03| 0:0:00| chol  1  1
   * 2|0.816|0.925|3.5e+00|4.8e-01|2.2e+04| 3.677791e+03 -5.782595e+03| 0:0:00| chol  1  1
   * 3|1.000|1.000|3.2e-07|3.0e-03|3.3e+03| 7.715993e+02 -2.484738e+03| 0:0:00| chol  1  1
   * 4|1.000|1.000|6.5e-08|3.0e-04|1.4e+03| 5.893026e+02 -8.053441e+02| 0:0:00| chol  1  1
   * 5|0.943|0.930|6.5e-09|4.9e-05|1.2e+02| 2.874052e+01 -8.755527e+01| 0:0:00| chol  1  1
   * 6|1.000|0.934|8.9e-10|6.0e-06|4.8e+01| 1.293684e+01 -3.528566e+01| 0:0:00| chol  1  1
   * 7|0.838|1.000|2.5e-10|3.0e-07|2.1e+01| 6.966577e+00 -1.359728e+01| 0:0:00| chol  1  1
   * 8|1.000|1.000|4.5e-11|3.0e-08|7.3e+00| 2.161033e+00 -5.142484e+00| 0:0:00| chol  1  1
   * 9|0.956|1.000|2.2e-11|3.0e-09|1.5e+00| 4.750848e-01 -9.875244e-01| 0:0:00| chol  1  1
   * 10|1.000|0.981|7.1e-16|3.6e-10|2.0e-01| 5.394076e-02 -1.508425e-01| 0:0:00| chol  1  1
   * 11|0.948|1.000|1.5e-15|3.1e-11|4.0e-02| 1.317058e-02 -2.642490e-02| 0:0:01| chol  1  1
   * 12|0.930|1.000|3.8e-15|4.0e-12|8.1e-03| 2.718091e-03 -5.362711e-03| 0:0:01| chol  1  1
   * 13|1.000|0.959|3.5e-15|1.5e-12|1.8e-03| 4.772826e-04 -1.287088e-03| 0:0:01| chol  1  1
   * 14|0.913|0.967|5.0e-14|1.1e-12|2.5e-04| 8.269936e-05 -1.654022e-04| 0:0:01| chol  1  1
   * 15|1.000|1.000|4.2e-13|1.0e-12|8.7e-05| 2.561643e-05 -6.183946e-05| 0:0:01| chol  1  1
   * 16|1.000|1.000|5.2e-12|1.0e-12|2.0e-05| 5.727135e-06 -1.454375e-05| 0:0:01| chol  1  1
   * 17|1.000|1.000|1.9e-12|1.0e-12|3.2e-06| 9.146066e-07 -2.245227e-06| 0:0:01| chol  1  1
   * 18|1.000|1.000|5.3e-11|1.0e-12|5.8e-07| 1.648380e-07 -4.138199e-07| 0:0:01| chol  2  2
   * 19|1.000|1.000|1.5e-13|1.5e-12|1.2e-07| 3.331824e-08 -8.154610e-08| 0:0:01| chol  2  2
   * 20|1.000|1.000|3.4e-13|1.0e-12|2.6e-08| 7.293253e-09 -1.836591e-08| 0:0:01| chol  2  2
   * 21|1.000|1.000|7.3e-13|1.0e-12|5.0e-09| 1.410000e-09 -3.442996e-09| 0:0:01|
   * stop: max(relative gap, infeasibilities) < 1.49e-08
   * -------------------------------------------------------------------
   * number of iterations   = 21
   * primal objective value =  1.40999951e-09
   * dual   objective value = -3.44299564e-09
   * gap := trace(XZ)       = 5.04e-09
   * relative gap           = 5.04e-09
   * actual relative gap    = 4.85e-09
   * rel. primal infeas     = 7.31e-13
   * rel. dual   infeas     = 1.00e-12
   * norm(X), norm(y), norm(Z) = 1.9e+01, 1.6e-05, 1.1e+01
   * norm(A), norm(b), norm(C) = 3.1e+01, 2.4e+01, 1.2e+01
   * Total CPU time (secs)  = 1.03
   * CPU time per iteration = 0.05
   * termination code       =  0
   * DIMACS: 5.7e-12  0.0e+00  6.2e-12  0.0e+00  4.9e-09  5.0e-09
   * -------------------------------------------------------------------
   * ------------------------------------------------------------
   * Status: Solved
   * Optimal value (cvx_optval): +1.41e-09
   *
   */

  val votingExample2 = """
	predicate[PartialFunctional]: 	votes(_, _)
	predicate[Symmetric]: 		friends(_, _)
    predicate[Symmetric]: 		enemies(_, _)

    rule [weight = 1]: 	votes(A,P) && friends(A,B) => votes(B,P) 
    rule [weight = 1]: 	votes(A,P) && enemies(A,B) => !votes(B,P) 
    rule: 	enemies(A,B) => !friends(A,B) 
    rule: 	friends(A,B) => !enemies(A,B)  
    
	fact: friends(anna, bob)
    fact: friends(daria, bob)
	fact [truthValue = 0.8]: votes(anna, democrats)
    fact [truthValue = 0.2]: votes(carl, repub)
    fact [truthValue = 0.7]: votes(daria, repub)
    fact [truthValue = 0.7]: votes(enrico, greenparty)
    fact [truthValue = 0.8]: enemies(carl, bob)
	"""
  
  //TODO(sara): This test sometimes fails because it ends up violating a constraint.
//  "VotingExample" should "provide a solution consistent with Matlab also with more constraints" in {
//    val pslData = PslParser.parse(votingExample2)
//    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 10e-10, relativeEpsilon = 10e-5, isBounded = true,
//      removeSymmetricConstraints = false)
//    val inferenceResults = Inferencer.runInference(pslData, config = config)
//
//    val solution = inferenceResults.solution
//    val gps = inferenceResults.idToGpMap
//    val objectiveFunctionVal = inferenceResults.objectiveFun.get
//
//    println(inferenceResults)
//
//    objectiveFunctionVal should be(0.0 +- 5e-5)
//  }
}
