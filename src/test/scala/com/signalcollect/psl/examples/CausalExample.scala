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

class CausalExample extends FlatSpec with Matchers with TestAnnouncements {

  val casual = """
// As first we show an example with three variables.
// We can extend to more variables, if we add the possibility of having sets of constants of a 
// specific class as arguments (monadic second order logic with Henkin semantics).
// See the second example for an idea of how the logic should look.

// ********************************************************************************************
//  Example with four variables
// ********************************************************************************************
// The two arguments are independent of each other.
predicate [Symmetric]: indep(Variable, Variable)
// The two first arguments are conditionally independent based on the third argument.
predicate : cond-indep(Variable, Variable, Variable)
// The first argument causes the second argument.
// We don’t add not-causes, but instead use !causes, we will add a mechanism to deal if there 
// is no evidence for neither.
predicate : causes(Variable, Variable)

// 0. conditional independence is symmetric in the first two variables.
rule: cond-indep(X, Y, Z) => cond-indep(Y, X, Z)

// 1. mutual exclusivity of independence and dependence.
// ! (( X → Y) && (X -/-> Y))
// We assume it’s implicit by using the negation: !causes(X, Y) \equiv not-causes(X,Y).

// 2. Irreflexivity of causes:
// !(X → X)
rule: !causes(X, X)

// 3. Irreflexivity of not causes:
// X -/-> X
// We assume it’s implicit by using the negation: !causes(X, Y) \equiv not-causes(X,Y).

// 4. Acyclicity
// X→ Y => Y-/->X
rule: causes(X,Y) => !causes(Y,X)

// 5. Transitivity:
// X → Y && Y → Z => X → Z
rule: causes(X,Y)  && causes(Y,Z) => causes(X,Z)

// 6. If Z makes X and Y conditionally independent, then Z causes either X or Y or both.
rule: !indep(X,Y)  && cond-indep(X,Y,Z) => causes(Z, X) || causes (Z, Y)

// 7. If Z makes X and Y conditionally dependent, then Z does not cause neither X or Y.
// rule [ weight = 1]: indep(X,Y)  && !cond-indep(X,Y,Z) => !causes(Z, X) && !causes (Z, Y)
// we rewrite it in two different rules, since &&s are not supported on the right side of the rule.
rule: indep(X,Y)  && !cond-indep(X,Y,Z) => !causes(Z, X)
rule: indep(X,Y)  && !cond-indep(X,Y,Z) => !causes(Z, Y)

// Some example facts:
// fact [truthValue = 0.8]: !indep(a, b)
// fact [truthValue = 0.99]: cond-indep(a, b, c)
// fact [truthValue = 0.7]: causes(c, d)

class Variable: u,w,x,y

fact: indep(w, u)
fact: !cond-indep(w, u, x)
fact: !indep(w, y)
fact: cond-indep(w, y, x)
  """

  it should "provide a solution consistent for hardenemies, an example with negative prior and a hard rule" in {
    val pslData = PslParser.parse(casual)
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    println(inferenceResults)
  }
}
