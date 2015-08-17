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
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.util.TestAnnouncements
import com.signalcollect.admm.utils.MinimaExplorer
import com.signalcollect.psl.translate.PSLToCvxConverter
import com.signalcollect.psl.translate.PSLToLPConverter

class DConnectionExample extends FlatSpec with Matchers with TestAnnouncements {

  val causal = """
class Variable: 0,1,2

// *** Predicates ***
// Different types of edges, tail tail, tail head, head head
// X, Y, C = conditioning set, J = intervention set, M = marginalization set
// The notations are equivalent, V of the paper notation is the set of original variables
// minus J and M. 
// C and M are disjoint, while J is not disjoint with them.
// You cannot intervene on a conditioned or marginalized variable, but you can condition/marginalize an intervened one.

predicate: th(Variable, Variable, Set[Variable], Set[Variable], Set[Variable])
predicate: hh(Variable, Variable, Set[Variable], Set[Variable], Set[Variable])
predicate: tt(Variable, Variable, Set[Variable], Set[Variable], Set[Variable])

// Csub,Z,C,J,M - Csub is original conditioning set, C the new one
predicate: condition(Variable)
// C,J,Msub,Z,M
predicate: marginalize(Variable)
// C,Jsub,Z,J,M - Jsub is original intervention set, J is the new one
predicate: intervene(Variable)

// X, Y, C = conditioning set, J = intervention set, M = marginalization set
predicate: indep(Variable, Variable, Set[Variable], Set[Variable], Set[Variable])

// Tempfix is a predicate that we need to have a binding to V (a variable representing extra vars that are not X,Y,Z or C)
// Currently in foxPSL it is not possible to vvhave a variable in a union operation ({}), which is not already defined
// somewhere else alone. This will be probably fixed in the future.
predicate: tempfix(Set[Variable])

// This rule says tempfix is always true, so it doesn't impact any of the rules.
rule: tempfix(V)
rule: intervene(Z)

// --------------------------------------------------------------------------------------------------
// *** Rules for interventions ***

// Allow tt(X,X, ...) paths
// tt(X,Y,C,J,M):- tt(X,Y,C,Jsub,M), X <= Y, not ismember(C,X), not ismember(C,Y), 
//    not ismember(M,X), not ismember(M,Y), intervene(C,Jsub,Z,J,M).
// Should be also not ismember(M, Z) , not ismember(C, Z) = cannot intervene on a marginalized or conditioned var.
//tt(X,Y,C,J,M) && intervene(Z) => tt(X,Y,C,{J,Z},M) 
//tt(X,X,C,J,M) && intervene(Z) => tt(X,X,C,{J,Z},M) 
// J is not necessarily disjoint with C or M, although C and M are among them. 
rule: tempfix(C) && tempfix(J1) && tempfix(J2) && tempfix(J3) && tempfix(M) && tt(X,Y,{C,J1},{J1,J2,J3},{M,J3}) && intervene(Z) => tt(X,Y,{C,J1},{J1,J2,J3,Z},{M,J3}) 
rule: tempfix(C) && tempfix(J1) && tempfix(J2) && tempfix(J3) && tempfix(M) && tt(X,X,{C,J1},{J1,J2,J3},{M,J3}) && intervene(Z) => tt(X,X,{C,J1},{J1,J2,J3,Z},{M,J3}) 

 """

  it should "provide a solution consistent for the causal example" in {
    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      lazyThreshold = None,
      removeSymmetricConstraints = false,
      tolerance = 0,
      breezeOptimizer = false,
      //verbose =  true,
      maxIterations = 1,
      absoluteEpsilon = 1e-5,
      relativeEpsilon = 1e-3)
    val inferenceResults = Inferencer.runInferenceFromString(causal, config = config)
    println(inferenceResults.objectiveFun)
    println(inferenceResults.printSelectedResults(List.empty, sortById = true, printOutZeros = true))

    //    // Experimental.
    //    val results = MinimaExplorer.exploreFromString(causal, config, List("causes"))
    //    for (result <- results) {
    //      if (result._3 == 0 && result._4 == 0) {
    //        println(s"${result._1}: false = ${result._2} : [${result._3},${result._4}]")
    //      } else if (result._3 == 1 && result._4 == 1) {
    //        println(s"${result._1}: true  = ${result._2} : [${result._3},${result._4}]")
    //      } else {
    //        println(s"${result._1}: unknown  = ${result._2} : [${result._3},${result._4}]")
    //      }
    //    }
  }
}
