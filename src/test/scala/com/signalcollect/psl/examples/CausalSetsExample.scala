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
import com.signalcollect.psl.model.PSLToCvxConverter

class CausalSetsExample extends FlatSpec with Matchers with TestAnnouncements {

  val causal = """
class Variable: x
// class SelectionNode: s1,s2,s3

predicate : indep(Variable, Variable, Set{0,3}[Variable])
predicate : causes(Variable, Variable)
predicate : notcauses(Variable, Variable)

// 0. conditional independence is symmetric in the first two variables.
rule: indep(X, Y, Z) => indep(Y, X, Z)

// 1. mutual exclusivity of independence and dependence.
// ! (( X → Y) && (X -/-> Y))
rule: notcauses(X,Y) => !causes(X,Y)

// 2. Irreflexivity of causes:
// !(X → X)
rule: !causes(X,X)

// 3. Irreflexivity of not causes:
// X -/-> X
rule: notcauses(X,X) 

// 4. Acyclicity
// X→ Y => Y-/->X
//rule: causes(X,Y) => !causes(Y,X)
rule: causes(X,Y) => notcauses(Y,X)


// 5. Transitivity:
// X → Y && Y → Z => X → Z
rule: causes(X,Y)  && causes(Y,Z) => causes(X,Z)

// 6. If Z makes X and Y conditionally independent, then Z causes either X or Y or both.
rule: !indep(X,Y,W)  && indep(X,Y,{W, Z}) => causes(Z, X) || causes (Z, Y) || EXISTS [W1 in W] causes (Z, W1)
// 6b. W cannot be empty, as we cannot do EXISTS on an empty set, so we need to add extra rule.
rule: !indep(X,Y,{})  && indep(X,Y,Z) => causes(Z, X) || causes (Z, Y) 

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y, nor any of W.
//rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, X)
//rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, Y)
//rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => FOREACH [W1 in W] !causes(Z, W1)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => notcauses(Z, X)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => notcauses(Z, Y)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => FOREACH [W1 in W] notcauses(Z, W1)

// 8. If X and Y are independent, then they are not causing each other.
// Faithfulness assumption.
//rule: indep(X,Y,{}) => !causes(X, Y)
rule: indep(X,Y,{}) => notcauses(X, Y)

// 9. Tom's new rule. 
// W is a Variable (causes (X, W) makes it a single variable) and cannot be empty.
// Z is a Set [Variable] and cannot be empty.
// X and Y become independent when we add W to Z.
//rule: indep(X,Y,Z) && FOREACH [Z1 strictSubsetOf Z] !indep(X,Y,Z1) && FOREACH [Z2 in Z] !causes(X, Z2) => !causes(X,Y)
rule: indep(X,Y,Z) && FOREACH [Z1 strictSubsetOf Z] !indep(X,Y,Z1) && FOREACH [Z2 in Z] notcauses(X, Z2) => notcauses(X,Y)

// True causal structure:
// w -> x <- u
//      x -> y

//fact: indep(w, u, {})
//fact: !indep(w, u, x)
//fact: !indep(w, y, {})
//fact: indep(w, y, x)
//fact: !indep(u, y, {})
//fact: indep(u, y, x)

//rule [100, linear]: indep(w, u, {})
//rule [100, linear]: !indep(w, u, x)
//rule [100, linear]: !indep(w, y, {})
//rule [100, linear]: indep(w, y, x)
//rule [100, linear]: !indep(u, y, {})
//rule [100, linear]: indep(u, y, x)

rule: !causes(X,w)
rule : !causes(X,u)

fact: !indep(x, u, {})
fact: !indep(x, w, {})
fact: !indep(x, y, {})
fact: !indep(y, u, {})
fact: !indep(y, w, {})
fact: indep(w, u, {})

fact: indep(u, y, x)
fact: indep(w, y, x)
fact: !indep(w, u, x)

fact: !indep(w, u, y)
fact: !indep(x, w, y)
fact: !indep(x, u, y)

fact: !indep(x, w, u)
fact: !indep(x, y, u)
fact: !indep(y, w, u)

fact: !indep(x, y, w)
fact: !indep(x, u, w)
fact: !indep(u, y, w)

fact: !indep(x, y, {u, w})
fact: !indep(x, w, {y, u})
fact: !indep(x, u, {y, w})
fact: !indep(u, w, {y, x})

fact: indep(u, y, {x, w})
fact: indep(w, y, {x, u})
 """

  it should "provide a solution consistent for the causal example" in {
    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      lazyThreshold = None,
      removeSymmetricConstraints = false,
      tolerance = 0,
      //breezeOptimizer = false,
      //verbose =  true,
      maxIterations = 200000,
      absoluteEpsilon = 1e-12,
      relativeEpsilon = 1e-8)
    val inferenceResults = Inferencer.runInferenceFromString(causal, config = config)
    println(inferenceResults.objectiveFun)
    println(inferenceResults.printSelected(List.empty))

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

    //    println(PSLToCvxConverter.toCvx(causal))
  }

  //rule [1]:!indep(x, u, {})
  //rule [1]:!indep(x, w, {})
  //rule [1]:!indep(x, y, {})
  //rule [1]:!indep(y, u, {})
  //rule [1]:!indep(y, w, {})
  //rule [0.1]: indep(w, u, {})
  //
  //rule [0.1]: indep(u, y, x)
  //rule [0.1]: indep(w, y, x)
  //rule [1]:!indep(w, u, x)
  //
  //rule [1]:!indep(w, u, y)
  //rule [1]:!indep(x, w, y)
  //rule [1]:!indep(x, u, y)
  //
  //rule [1]:!indep(x, w, u)
  //rule [1]:!indep(x, y, u)
  //rule [1]:!indep(y, w, u)
  //
  //rule [1]:!indep(x, y, w)
  //rule [1]:!indep(x, u, w)
  //rule [1]:!indep(u, y, w)
  //
  //rule [1]:!indep(x, y, {u, w})
  //rule [1]:!indep(x, w, {y, u})
  //rule [1]:!indep(x, u, {y, w})
  //rule [1]:!indep(u, w, {y, x})
  //
  //rule [0.1]: indep(u, y, {x, w})
  //rule [0.1]: indep(w, y, {x, u})

  //// Symmetric
  //rule [1]:!indep(u, x, {})
  //rule [1]:!indep(w, x, {})
  //rule [1]:!indep(y, x, {})
  //rule [1]:!indep(u, y, {})
  //rule [1]:!indep(w, y, {})
  //rule [0.1]: indep(u, w, {})
  //
  //rule [0.1]: indep(y, u, x)
  //rule [0.1]: indep(y, w, x)
  //rule [1]:!indep(u, w, x)
  //
  //rule [1]:!indep(u, w, y)
  //rule [1]:!indep(w, x, y)
  //rule [1]:!indep(u, x, y)
  //
  //rule [1]:!indep(w, x, u)
  //rule [1]:!indep(y, x, u)
  //rule [1]:!indep(w, y, u)
  //
  //rule [1]:!indep(y, x, w)
  //rule [1]:!indep(u, x, w)
  //rule [1]:!indep(y, u, w)
  //
  //rule [1]:!indep(y, x, {u, w})
  //rule [1]:!indep(w, x, {y, u})
  //rule [1]:!indep(u, x, {y, w})
  //rule [1]:!indep(w, u, {y, x})
  //
  //rule [0.1]: indep(y, u, {x, w})
  //rule [0.1]: indep(y, w, {x, u})

  val causalSimpleExampleSetsExact = """
predicate : indep(Variable, Variable, Set{0,1}[Variable])
predicate : causes(Variable, Variable)

// 0. conditional independence is symmetric in the first two variables.
rule: indep(X, Y, Z) => indep(Y, X, Z)

rule: !causes(X, X)
rule: causes(X,Y) => !causes(Y,X)
rule: causes(X,Y)  && causes(Y,Z) => causes(X,Z)

// 6. If Z makes X and Y conditionally independent in the context of W, then Z causes X, or Y, or W
// rule: !indep(X,Y,W)  && indep(X,Y,{W, Z}) => causes(Z, X) || causes (Z, Y) || !FOREACH [W1 in W] !causes (Z, W1)
rule: !indep(X,Y,W) && indep(X,Y,{W, Z}) => causes(Z, X) || causes (Z, Y) || EXISTS [W1 in W] causes (Z, W1)
// Missing: rule: !indep(X,Y,{})  && indep(X,Y,Z) => causes(Z, X) || causes (Z, Y) 

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y, nor any of W.
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, X)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, Y)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => FOREACH [W1 in W] !causes(Z, W1)

// 8. If X and Y are independent, then they are not causing each other.
// Faithfulness assumption.
rule: indep(X,Y,{}) => !causes(X, Y)

// 9. Tom's new rule. 
// W is a Variable (causes (X, W) makes it a single variable) and cannot be empty.
// Z is a Set [Variable] and cannot be empty.
// X and Y become independent when we add W to Z.
rule: indep(X,Y,Z) && FOREACH [Z1 strictSubsetOf Z] !indep(X,Y,Z1) && FOREACH [Z2 in Z] !causes(X, Z2) => !causes(X,Y)

// fact: indep(w, u, {})
// fact: !indep(w, u, x)
// fact: !indep(w, y, {})
// fact: indep(w, y, x)
// fact: !indep(u, y, {})
// fact: indep(u, y, x)

class Variable: u,w,x,y

fact: indep(w, u, {})
fact: !indep(w, u, x)
fact: !indep(w, y, {})
fact: indep(w, y, x)
fact: !indep(u, y, {})
fact: indep(u, y, x)

//rule : indep(w, u, {})
//rule : !indep(w, u, x)
//rule : !indep(w, y, {})
//rule : indep(w, y, x)
//rule : !indep(u, y, {})
//rule : indep(u, y, x)
"""
}
