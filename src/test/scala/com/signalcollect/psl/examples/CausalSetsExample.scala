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
class Variable: u,w,x,y
predicate : indep(Variable, Variable, Set[Variable])
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

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y.
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, X)
rule: indep(X,Y,W)  && !indep(X,Y,{W,Z}) => !causes(Z, Y)

// 8. If X and Y are independent, then they are not causing each other.
rule: indep(X,Y, {}) => !causes(X, Y)
//rule: indep(X,Y, {}) => !causes(Y, X)

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
fact: !indep(x, y, {w, u})

fact: !indep(x, w, {y, u})
fact: !indep(x, w, {u, y})

fact: !indep(x, u, {y, w})
fact: !indep(x, u, {w, y})

fact: !indep(u, w, {y, x})
fact: !indep(u, w, {x, y})

fact: indep(u, y, {x, w})
fact: indep(u, y, {w, x})

fact: indep(w, y, {x, u})
fact: indep(w, y, {u, x})


 """

  it should "provide a solution consistent for the causal example" in {
    val config = InferencerConfig(
        computeObjectiveValueOfSolution = true, 
        lazyThreshold = None, 
        removeSymmetricConstraints = false,
        maxIterations = 200000)
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
