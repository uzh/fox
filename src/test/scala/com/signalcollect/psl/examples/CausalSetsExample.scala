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
class Variable: u,w,x,y,z,a,b,c

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
rule: !indep(X,Y,W)  && indep(X,Y,{W, Z}) => causes(Z, X) || causes (Z, Y)

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y, nor any of W.
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, X)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, Y)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, W)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => FOREACH [W1 in W] !causes(Z, W1)

// 8. If X and Y are independent, then they are not causing each other.
// Faithfulness assumption.
rule: indep(X,Y, {}) => !causes(X, Y)
//rule: indep(X,Y, {}) => !causes(Y, X)

// 9. Tom's new rule. 
// TODO rule: !indep(X,Y,W)  && indep(X,Y,{W,Z}) && !causes(X, Z) && FORALL [W1 in W] !causes(X, W1) => !causes(X,Y)


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
fact: !indep(x, w, u)
fact: !indep(x, y, w)
fact: !indep(x, y, u)
fact: !indep(x, u, y)
fact: !indep(x, u, w)
fact: !indep(u, y, w)
fact: !indep(y, w, u)

fact: !indep(x, y, {u, w})
fact: !indep(x, w, {y, u})
fact: !indep(x, u, {y, w})
fact: !indep(u, w, {y, x})

fact: indep(u, y, {x, w})
fact: indep(w, y, {x, u})
 """
/*
 * GroundedPredicate 2921: causes[ ] (w, b): unknown  = 0.0 : [0.0,0.878]
GroundedPredicate 1554: causes[ ] (w, y): unknown  = 0.0 : [0.0,0.898]
GroundedPredicate 1406: causes[ ] (a, c): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 2622: causes[ ] (z, c): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 1437: causes[ ] (x, x): false = 0.0 : [0.0,0.0]
GroundedPredicate 1756: causes[ ] (w, w): false = 0.0 : [0.0,0.0]
GroundedPredicate 2046: causes[ ] (b, w): unknown  = 0.0 : [0.0,0.889]
GroundedPredicate 3088: causes[ ] (y, z): unknown  = 0.0 : [0.0,0.897]
GroundedPredicate 1518: causes[ ] (x, u): false = 0.0 : [0.0,0.0]
GroundedPredicate 3046: causes[ ] (x, w): false = 0.0 : [0.0,0.0]
GroundedPredicate 2403: causes[ ] (c, b): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 969: causes[ ] (u, y): unknown  = 0.0 : [0.0,0.898]
GroundedPredicate 1013: causes[ ] (z, b): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 348: causes[ ] (z, w): unknown  = 0.0 : [0.0,0.889]
GroundedPredicate 3189: causes[ ] (y, a): unknown  = 0.0 : [0.0,0.897]
GroundedPredicate 243: causes[ ] (w, a): unknown  = 0.0 : [0.0,0.878]
GroundedPredicate 512: causes[ ] (a, y): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 3154: causes[ ] (u, u): false = 0.0 : [0.0,0.0]
GroundedPredicate 757: causes[ ] (w, c): unknown  = 0.0 : [0.0,0.878]
GroundedPredicate 2895: causes[ ] (w, x): unknown  = 0.0 : [0.0,0.896]
GroundedPredicate 1121: causes[ ] (u, b): unknown  = 0.0 : [0.0,0.877]
GroundedPredicate 403: causes[ ] (z, u): unknown  = 0.0 : [0.0,0.869]
GroundedPredicate 1567: causes[ ] (a, x): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 124: causes[ ] (y, x): false = 0.0 : [0.0,0.0]
GroundedPredicate 2505: causes[ ] (y, w): false = 0.0 : [0.0,0.0]
GroundedPredicate 721: causes[ ] (c, z): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 953: causes[ ] (u, x): unknown  = 0.0 : [0.0,0.859]
GroundedPredicate 2292: causes[ ] (b, a): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 2556: causes[ ] (c, x): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 1884: causes[ ] (u, a): unknown  = 0.0 : [0.0,0.877]
GroundedPredicate 27: causes[ ] (a, w): unknown  = 0.0 : [0.0,0.889]
GroundedPredicate 3002: causes[ ] (u, z): unknown  = 0.0 : [0.0,0.877]
GroundedPredicate 3170: causes[ ] (b, z): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 2637: causes[ ] (c, u): unknown  = 0.0 : [0.0,0.869]
GroundedPredicate 1117: causes[ ] (w, z): unknown  = 0.0 : [0.0,0.878]
GroundedPredicate 2871: causes[ ] (u, c): unknown  = 0.0 : [0.0,0.877]
GroundedPredicate 3073: causes[ ] (x, z): unknown  = 0.0 : [0.0,0.855]
GroundedPredicate 483: causes[ ] (a, a): false = 0.0 : [0.0,0.0]
GroundedPredicate 1113: causes[ ] (y, b): unknown  = 0.0 : [0.0,0.897]
GroundedPredicate 1076: causes[ ] (b, x): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 2952: causes[ ] (c, c): false = 0.0 : [0.0,0.0]
GroundedPredicate 648: causes[ ] (c, w): unknown  = 0.0 : [0.0,0.889]
GroundedPredicate 1081: causes[ ] (x, c): unknown  = 0.0 : [0.0,0.855]
GroundedPredicate 3019: causes[ ] (b, u): unknown  = 0.0 : [0.0,0.869]
GroundedPredicate 2224: causes[ ] (b, b): false = 0.0 : [0.0,0.0]
GroundedPredicate 1038: causes[ ] (z, y): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 127: causes[ ] (x, y): true  = 1.0 : [1.0,1.0]
GroundedPredicate 1285: causes[ ] (z, x): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 1141: causes[ ] (a, u): unknown  = 0.0 : [0.0,0.869]
GroundedPredicate 1272: causes[ ] (y, u): false = 0.0 : [0.0,0.0]
GroundedPredicate 1240: causes[ ] (z, z): false = 0.0 : [0.0,0.0]
GroundedPredicate 845: causes[ ] (b, c): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 1575: causes[ ] (u, w): false = 0.0 : [0.0,0.0]
GroundedPredicate 2823: causes[ ] (a, b): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 58: causes[ ] (y, c): unknown  = 0.0 : [0.0,0.897]
GroundedPredicate 2315: causes[ ] (y, y): false = 0.0 : [0.0,0.0]
GroundedPredicate 1116: causes[ ] (b, y): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 2517: causes[ ] (a, z): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 801: causes[ ] (z, a): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 3076: causes[ ] (w, u): false = 0.0 : [0.0,0.0]
GroundedPredicate 3013: causes[ ] (c, a): unknown  = 0.0 : [0.0,0.888]
GroundedPredicate 2672: causes[ ] (x, b): unknown  = 0.0 : [0.0,0.855]
GroundedPredicate 469: causes[ ] (c, y): unknown  = 0.0 : [0.0,1.0]
GroundedPredicate 1812: causes[ ] (x, a): unknown  = 0.0 : [0.0,0.855]
 */
  it should "provide a solution consistent for the causal example" in {
    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      lazyThreshold = None,
      removeSymmetricConstraints = false,
      maxIterations = 200000,
      absoluteEpsilon = 1e-5,
      relativeEpsilon = 1e-3)
    val results = MinimaExplorer.exploreFromString(causal, config, List("causes"))
    for (result <- results) {
      if (result._3 == 0 && result._4 == 0) {
        println(s"${result._1}: false = ${result._2} : [${result._3},${result._4}]")
      } else if (result._3 == 1 && result._4 == 1) {
        println(s"${result._1}: true  = ${result._2} : [${result._3},${result._4}]")
      } else {
        println(s"${result._1}: unknown  = ${result._2} : [${result._3},${result._4}]")
      }
    }
  }
}
