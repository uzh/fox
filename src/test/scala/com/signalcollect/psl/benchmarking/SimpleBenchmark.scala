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

package com.signalcollect.psl.benchmarking

import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile

import java.io.File

object SimpleBenchmark extends App {

  val config = InferencerConfig( lazyThreshold = None
  //,globalConvergenceDetection = None,
  )

  // For all the files in the small-benchmarks folder, run the examples $repetition times.
  val directory = new File("src/test/scala/com/signalcollect/psl/benchmarking/small-benchmarks/")
  val repetitions = 10
  val warmup = 5

  val exampleToAvgTime: Map[String, Double] = directory.listFiles().flatMap {
    exampleFile =>
      var i = 0
      var cumulativeTime = 0.0
      val parsedFile = PslParser.parse(exampleFile)
      while (i < warmup) {
        timedInference(parsedFile, config)
        i = i + 1
      }
      i = 0
      while (i < repetitions) {
        cumulativeTime += timedInference(parsedFile, config)
        i = i + 1
      }
      Map(exampleFile.getName() -> cumulativeTime / repetitions)
  }.toMap
  
  println(exampleToAvgTime)

  def timedInference(parsedFile: ParsedPslFile, config: InferencerConfig): Double = {
    val startTime = System.currentTimeMillis
    val inferenceResults = Inferencer.runInference(
      parsedFile, config = config)
    System.currentTimeMillis - startTime
  }

}
