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

package com.signalcollect.admm.optimizers

import com.signalcollect.util.IntDoubleHashMap

// A function to optimize with the ADMM algorithm, has to define an optimize() function.
trait OptimizableFunction {
  def id: Option[Int]
  def optimize(consensusAssignments: Map[Int, Double]): Map[Int, Double] = {
    optimizeEfficient(mapToArray(consensusAssignments))
    arrayToMap(getX)
  }
  def getY: Map[Int, Double] = arrayToMap(getYEfficient)
  def setY(y: Array[Double]) // check that the indexes are fine.
  def updateLagrange(zMap: Map[Int, Double]) {
    updateLagrangeEfficient(mapToArray(zMap))
  }
  def setZMap(zMap: Map[Int, Double]) {
    setZ(mapToArray(zMap))
  }

  def evaluateAt(m: IntDoubleHashMap): Double = {
    val mappings = idToIndexMappings
    val l = mappings.length
    val mapped = new Array[Double](l)
    var i = 0
    while (i < l) {
      val idForIndexI = mappings(i)
      mapped(i) = m(idForIndexI)
      i += 1
    }
    evaluateAtEfficient(mapped)
  }

  def evaluateAt(x: Map[Int, Double]): Double = evaluateAtEfficient(mapToArray(x))

  // Efficient functions.
  def setZ(z: Array[Double])
  def updateLagrangeEfficient(z: Array[Double])
  def evaluateAtEfficient(x: Array[Double]): Double
  def getYEfficient: Array[Double]
  def getX: Array[Double]
  def optimizeEfficient(consensus: Array[Double])
  def getStepSize: Double
  def setStepSize(stepSize: Double)
  def idToIndexMappings: Array[Int]

  // Helper function to convert from a map to a properly ordered array.
  @inline final def mapToArray(m: Map[Int, Double]): Array[Double] = {
    val mappings = idToIndexMappings
    val l = mappings.length
    val mapped = new Array[Double](l)
    var i = 0
    while (i < l) {
      val idForIndexI = mappings(i)
      mapped(i) = m(idForIndexI)
      i += 1
    }
    mapped
  }

  @inline final def arrayToMap(a: Array[Double]): Map[Int, Double] = {
    idToIndexMappings.zip(a).toMap
  }

}
