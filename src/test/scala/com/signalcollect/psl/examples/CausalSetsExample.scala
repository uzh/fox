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

class CausalSetsExample extends FlatSpec with Matchers with TestAnnouncements {

  val causal = """
class Variable
// class SelectionNode: s1,s2,s3

predicate : indep(Variable, Variable, Set{0,3}[Variable])
predicate : causes(Variable, Variable)

// 0. conditional independence is symmetric in the first two variables.
rule: indep(X, Y, Z) => indep(Y, X, Z)

// 2. Irreflexivity of causes:
// !(X → X)
rule: !causes(X, X)

// 4. Acyclicity
// X→ Y => Y-/->X
rule: causes(X,Y) => !causes(Y,X)

// 5. Transitivity:
// X → Y && Y → Z => X → Z
rule: causes(X,Y)  && causes(Y,Z) => causes(X,Z)

// 6. If Z makes X and Y conditionally independent, then Z causes either X or Y or both.
rule: !indep(X,Y,W)  && indep(X,Y,{W, Z}) => causes(Z, X) || causes (Z, Y) || EXISTS [W1 in W] causes (Z, W1)

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y, nor any of W.
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, X)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, Y)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, W)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => FOREACH [W1 in W] !causes(Z, W1)

// 8. If X and Y are independent, then they are not causing each other.
// Faithfulness assumption.
rule: indep(X,Y, {}) => !causes(X, Y)

// 9. Tom's new rule. 
// W is a Variable (causes (X, W) makes it a single variable).
// Z is a Set [Variable]
// X and Y become independent when we add W to Z.
rule: !indep(X,Y,Z) && !indep(X,Y,W) && indep(X,Y,{Z,W}) && FOREACH [Z1 in Z] !causes(X, Z1) && !causes(X, W) => !causes(X,Y)


rule [1]: !indep(x, u, {})
rule [1]: !indep(x, w, {})
rule [1]: !indep(x, y, {})
rule [1]: !indep(y, u, {})
rule [1]: !indep(y, w, {})
rule [1]: indep(w, u, {})

rule [1]: indep(u, y, x)
rule [1]: indep(w, y, x)

rule [1]: !indep(w, u, x)
rule [1]: !indep(w, u, y)
rule [1]: !indep(x, w, y)
rule [1]: !indep(x, w, u)
rule [1]: !indep(x, y, w)
rule [1]: !indep(x, y, u)
rule [1]: !indep(x, u, y)
rule [1]: !indep(x, u, w)
rule [1]: !indep(u, y, w)
rule [1]: !indep(y, w, u)

rule [1]: !indep(x, y, {u, w})
rule [1]: !indep(x, w, {y, u})
rule [1]: !indep(x, u, {y, w})
rule [1]: !indep(u, w, {y, x})

rule [1]: indep(u, y, {x, w})
rule [1]: indep(w, y, {x, u})
 """

  it should "provide a solution consistent for the causal example" in {
    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      lazyThreshold = None,
      removeSymmetricConstraints = false,
      maxIterations = 200000,
      absoluteEpsilon = 1e-5,
      relativeEpsilon = 1e-3)
    val inferenceResults = Inferencer.runInferenceFromString(causal, config = config)
    println(inferenceResults.printSelected(List.empty))
    // Experimental.
    //    val results = MinimaExplorer.exploreFromString(causal, config, List("none"))
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
