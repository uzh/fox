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
import com.signalcollect.admm.utils.Timer

import java.io.File

object SimpleBenchmark extends App {

  val config = InferencerConfig(maxIterations = 1)

  // For all the files in the small-benchmarks folder, run the examples $repetition times.
  val directory = new File("src/test/scala/com/signalcollect/psl/benchmarking/small-benchmarks/")
  val repetitions = 10
  val warmup = 10

  val exampleToAvgTime: Map[String, (Double, Double, Double, Double, Double, Double)] = {
    val files = directory.listFiles()
    val fileNames = files.map { x =>
      val indexOf = x.getName().indexOf("_of_")
      if (indexOf == -1) {
        x.getName()
      } else {
        x.getName().substring(0, indexOf - 1)
      }
    }.toList.distinct
    println(fileNames)
    fileNames.flatMap {
      exampleFileName =>
        var i = 0
        var cumulativeTime = 0.0
        var cumulativeGroundingTime = 0.0
        var cumulativeParsingTime = 0.0
        var minTime = Double.MaxValue
        var minGroundingTime = Double.MaxValue
        var minParsingTime = Double.MaxValue
        val exampleFile = new File(directory, exampleFileName)
        while (i < warmup) {
          if (exampleFile.exists()) {
            Inferencer.runInferenceFromFile(exampleFile, config = config)
          } else {
            // Fragmented file.
            val fragments = directory.list().filter(_.contains(exampleFileName)).map(new File(directory, _)).toList
            Inferencer.runInferenceFromFiles(fragments, config = config)
          }

          i = i + 1
        }
        i = 0
        while (i < repetitions) {
          val (inferenceResults, currentTime) = Timer.time {
            if (exampleFile.exists()) {
              Inferencer.runInferenceFromFile(exampleFile, config = config)
            } else {
              // Fragmented file.
              val fragments = directory.list().filter(_.contains(exampleFileName)).map(new File(directory, _)).toList
              Inferencer.runInferenceFromFiles(fragments, config = config)
            }
          }
          cumulativeTime += currentTime
          minTime = math.min(currentTime, minTime)
          cumulativeGroundingTime += inferenceResults.groundingTime.getOrElse(0L)
          minGroundingTime = math.min(inferenceResults.groundingTime.getOrElse(0L), minGroundingTime)
          cumulativeParsingTime += inferenceResults.parsingTime.getOrElse(0L)
          minParsingTime = math.min(inferenceResults.parsingTime.getOrElse(0L), minParsingTime)
          i = i + 1
        }
        Map(exampleFile.getName() ->
          (cumulativeTime / repetitions, minTime, 
              cumulativeGroundingTime / repetitions, minGroundingTime, 
              cumulativeParsingTime / repetitions, minParsingTime))
    }.toMap

  }

  println(exampleToAvgTime)

}
