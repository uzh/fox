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

import breeze.linalg.DenseVector
import breeze.optimize.DiffFunction
import breeze.optimize.minimize

class LinearLossOptimizer(
  setId: Int,
  val weight: Double,
  constant: Double,
  zIndices: Array[Int],
  stepSize: Double = 1.0,
  initialZmap: Map[Int, Double],
  coefficientMatrix: Array[Double]) extends OptimizerBase(setId, constant, zIndices, stepSize, initialZmap, coefficientMatrix) {

  lazy val linearlossFunction = {
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        //weight * coeffs^T * x  + stepSize/2 * norm2(x - z + (y / stepSize))^2
        (coeffsDotX * weight + stepSize / 2 * norm2WithoutSquareRoot(x - z + y / stepSize),
          // gradient of function above.
          coeffs * weight + (x - z) * stepSize + y)
      }
    }
  }

  def basicFunction = {
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        // weight * coeffs^T * x
        (coeffsDotX * weight,
        // gradient of function above.
         coeffs * weight)
      }
    }
  }

  def evaluateAtEfficient(someX: Array[Double]): Double = {
    basicFunction.valueAt(DenseVector(someX))
  }

  def gradientAt(xMap: Map[Int, Double]): Map[Int, Double] = {
    zIndices.zip(basicFunction.gradientAt(DenseVector(zIndices.map(xMap))).data).toMap
  }

  /**
   * Adaptation of Stephen Bach's solver.
   *
   * Objective term of the form
   * weight * coeffs^T * x
   */
  def optimizeEfficient(
    consensusAssignments: Array[Double]) {
    setZ(consensusAssignments)
    x = z - (y / stepSize)
    x = x - coeffs * weight / stepSize
//    x = minimize(linearlossFunction, x)
  }

  override def toString = s"LinearLossOptimizer(x=$x, y=$y, z=$z, coeffs=$coeffs, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
