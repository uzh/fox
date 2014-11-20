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

class SetExample extends FlatSpec with Matchers with TestAnnouncements {

  val causal = """
predicate : causes(Set[Variable], Set[Variable])

//rule: !causes(X, X)
rule: causes(X ,Y) => causes(Y,X)
rule: causes(X,Y)  && causes(X,Z) => causes(X,{Y,Z})

class Variable: x,y,z

fact: causes({x, z}, y)
//fact[0.3]: !causes(y, {w, u})
//fact[0.7]: !causes({x, y} , u)
  """

  it should "provide a solution consistent for the set example" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, lazyThreshold = None)
    val results = Inferencer.runInferenceFromString(causal, config = config)
    println(results)
  }
}
