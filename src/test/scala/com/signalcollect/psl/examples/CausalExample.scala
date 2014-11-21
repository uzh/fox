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
// The two arguments are independent of each other.
//predicate [Symmetric]: indep(Variable, Variable)
predicate : indep(Variable, Variable)
// The two first arguments are conditionally independent based on the third argument.
predicate : cond-indep(Variable, Variable, Variable)
// The first argument causes the second argument.
// We don’t add not-causes, but instead use !causes, we will add a mechanism to deal if there 
// is no evidence for neither.
predicate : causes(Variable, Variable)

// 0. conditional independence is symmetric in the first two variables.
//rule: cond-indep(X, Y, Z) => cond-indep(Y, X, Z)

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
rule: indep(X,Y)  && !cond-indep(X,Y,Z) => !causes(Z, X)
rule: indep(X,Y)  && !cond-indep(X,Y,Z) => !causes(Z, Y)

class Variable: u,w,x,y

fact: indep(w, u)
fact: !cond-indep(w, u, x)
fact: !indep(w, y)
fact: cond-indep(w, y, x)
  """
  
  val expected = """

fact: indep(w, u)
fact [0]: indep(w, y)

fact [0]: cond-indep(w, u, x)
fact: cond-indep(w, y, x)

fact [0]: causes(x,w) //ok
fact [0]: causes(x,u) //ok
fact [1]: causes(x,y) // w and y are dependent, but they are not anymore given x.
fact [0]: causes(y, x) //ok
fact [0]: causes(y,w)  //ok
fact [0]: causes(y,u) //ok

GroundedPredicate 166: causes[ ] (u, x)  has truth value 0.46
GroundedPredicate 170: causes[ ] (w, x)  has truth value 0.42539530927390246
GroundedPredicate 82: causes[ ] (w, y)  has truth value 0.4254391646493712
GroundedPredicate 119: causes[ ] (u, y)  has truth value 0.46

"""
//fact: indep(w, u)
////fact: indep( u, w) 
//fact [0]: indep(w, y)
////fact [0]: indep(y, w)
//
//fact [0]: cond-indep(w, u, x)
////fact [0]: cond-indep( u, w, x)
//fact: cond-indep(w, y, x)
////fact: cond-indep(y,w, x)
//
//// Rule 7:
//
//rule: MAX(0, indep(w, u):1 + !cond-indep(w, u, x):1 -1 - 1 + causes(x, w))^2 
//rule: MAX(0,  causes(x, w))^2 => causes(x,w) is 0
////same: rule: MAX(0, indep(u, w):1 + cond-indep(u, w, x):1 -1 - 1 + causes(x, w))^2 
//
//rule: MAX(0, indep(w, u):1 + !cond-indep(w, u, x):1 -1 - 1 + causes(x, u))^2 
//rule: MAX(0, causes(x, u))^2 => causes(x,u) is 0
//
//rule: MAX(0, indep(w, y):1 + !cond-indep(w, y, x):0 -1 - 1 + causes(x, w))^2 
//rule: MAX(0, -1 + causes(x, w))^2 => causes(x,w) is anything in [0, 1], but for other rule is 0
//
//rule: MAX(0, indep(w, y):1 + !cond-indep(w, y, x):0 -1 - 1 + causes(x, y))^2 
//rule: MAX(0, -1 + causes(x, y))^2 => causes(x,y) is anything in [0, 1]
//
//// Rule 6:
//
//rule:  MAX(0, !indep(w, u):0 + cond-indep(w, u, x):0 -1 - causes(x, w) - causes(x, u))^2 
//rule:  MAX(0, -1 - causes(x, w) - causes(x, u))^2  => causes(x,w|u) is anything in [0, 1], but for other rule is 0
//
//rule:  MAX(0, !indep(w, y):1 + cond-indep(w, y, x):1 -1 - causes(x, w) - causes(x, y))^2 
//rule:  MAX(0, 1 - causes(x, w) - causes(x, y))^2  => causes(x,w) +  causes(x, y) = 1, 
//// since causes(x,w) = 0, then causes(x,y) = 1
//
//// After rule 7 and 6
//fact [0]: causes(x,w)
//fact [0]: causes(x,u)
//fact [1]: causes(x,y) // w and y are dependent, but they are not anymore given x.
//
//// 4. Acyclicity
//
//rule: MAX(0, causes(x, w):0 - 1 + causes(w, x))^2
//rule: MAX(0, - 1 + causes(w, x))^2 => causes (w, x) can be anything in [0, 1]
//
//rule: MAX(0, causes(x, u):0 - 1 + causes(u, x))^2
//rule: MAX(0, - 1 + causes(u, x))^2 => causes (u, x) can be anything in [0, 1]
//
//rule: MAX(0, causes(x, y):1 - 1 + causes(y, x))^2
//rule: MAX(0, causes(y, x))^2 => causes (y, x) is 0
//
//fact [0]: causes(y, x)
//
//// 5. Transitivity:
//rule: MAX(0, causes(x, w):0 + causes(w, u) - 1 - causes(x, u): 0)^2
//rule: MAX(0,  causes(w, u) - 1 )^2 => causes (w, u) can be anything in [0, 1]
//
//rule: MAX(0, causes(x, w):0 + causes(w, y) - 1 - causes(x, y): 1)^2
//rule: MAX(0,  causes(w, y) - 2 )^2 => causes (w, y) can be anything in [0, 1]
//
//rule: MAX(0, causes(x, y):1 + causes(y, w) - 1 - causes(x, w): 0)^2
//rule: MAX(0,  causes(y, w)  )^2 => causes (y, w) is 0 
//
//rule: MAX(0, causes(x, y):1 + causes(y, u) - 1 - causes(x, u): 0)^2
//rule: MAX(0,  causes(y, u)  )^2 => causes (y, u) is 0
//
//fact [0]: causes(y,w)
//fact [0]: causes(y,u)


  it should "provide a solution consistent for hardenemies, an example with negative prior and a hard rule" in {
    val pslData = PslParser.parse(casual)
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    // println(inferenceResults.printSelected(List("causes")))
  }
}
