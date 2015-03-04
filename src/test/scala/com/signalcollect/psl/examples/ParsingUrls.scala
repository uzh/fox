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

class ParsingUrls extends FlatSpec with Matchers with TestAnnouncements {

  val urls = """
predicate : <http://causes.com> (<http://a123Cc>, <http://www.variable.com/Variable>)

// The only constraint is that variables have to start uppercase.
rule : <http://causes.com> (X,Y)

class <http://www.variable.com/Variable>
class <http://a123Cc>: b, <http://www.google.com>

rule [0.1]: !<http://causes.com>(x, <http://www.google.com>)

  """

  it should "provide a solution for the case with URLs" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true,
      lazyThreshold = None, absoluteEpsilon = 0, relativeEpsilon = 0)
    val inferenceResults = Inferencer.runInferenceFromString(urls, config = config)
    println(inferenceResults)
  }
}
