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

class SetExample extends FlatSpec with Matchers with TestAnnouncements {

  val causal = """
predicate : causes(Set{1,1}[Variable], Set {1, 4} [Variable])

//rule: !causes(X, X)
//rule: causes(X ,Y) => causes(Y,X)
rule : !causes(X,Y)

class Variable: x,y,z

rule [8, distanceMeasure = linear]: causes(x, {y,z})
rule [100, distanceMeasure = linear]: !causes(x, z)


//fact[0.3]: !causes(y, {w, u})
//fact[0.7]: !causes({x, y} , u)
  """

  it should "provide a solution consistent for the set example" in {
    val config = InferencerConfig(
      computeObjectiveValueOfSolution = true,
      lazyThreshold = None,
      removeSymmetricConstraints = false,
      maxIterations = 20000,
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
