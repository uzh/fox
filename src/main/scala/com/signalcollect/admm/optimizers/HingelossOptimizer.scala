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

class HingeLossOptimizer(
  setId: Int,
  val weight: Double,
  constant: Double,
  zIndices: Array[Int],
  stepSize: Double = 1.0,
  initialZmap: Map[Int, Double],
  coefficientMatrix: Array[Double]) extends OptimizerBase(setId, constant, zIndices, stepSize, initialZmap, coefficientMatrix) {

  lazy val hingeLossFunction = {
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        //weight * max(coeffs^T * x - constant, 0) + stepSize/2 * norm2(x - z + (y / stepSize))^2
        (math.max(coeffsDotX - constant, 0) * weight + stepSize / 2 * norm2WithoutSquareRoot(x - z + y / stepSize),
          // gradient of function above.
          if (constant < coeffsDotX) {
            coeffs * weight + (x - z) * stepSize + y
          } else {
            (x - z) * stepSize + y
          })
      }
    }
  }

  def basicFunction = {
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        //weight * max(coeffs^T * x - constant, 0)
        (math.max(coeffsDotX - constant, 0) * weight,
          // gradient of function above.
          if (constant < coeffsDotX) {
            // weight * [ c_0*x_0  + c_1*x_1 + ... + c_n*x_n - constant ]
            // df/dx_0 = weight*c_0
            coeffs * weight
          } else {
            DenseVector.zeros(coeffs.length)
          })
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
   * weight * max(coeffs^T * x - constant, 0)
   */
  def optimizeEfficient(
    consensusAssignments: Array[Double]) {
    setZ(consensusAssignments)
    //    val newXIfNoLoss = z - (y / stepSize)
    //    val total = coeffs.dot(newXIfNoLoss)
    //    x = newXIfNoLoss
    //    if (total <= constant) {
    //      return
    //    }
    //
    //    // Also consider linear loss:
    //    // argmin(weight * (coeffs^T * x - constant)+ stepSize/2 * norm2(x - z + (y / stepSize))^2)
    //    x = x - coeffs * weight / stepSize
    //    val linearLossTotal = coeffs.dot(x)
    //    if (linearLossTotal <= constant) {
    //      return
    //    }
    //
    //    // Else the solution is on the hinge.
    //    if (x.length == 1) {
    //      x(0) = constant / coeffs(0)
    //      return
    //    }
    //    // Project x onto coeffsDotX == constant plane.
    //    var distance = -constant / length
    //    distance += x.dot(unitNormalVector)
    //    x = x - (unitNormalVector * distance)

    x = minimize(hingeLossFunction, x)
  }

  override def toString = s"HingeLossOptimizer(x=$x, y=$y, z=$z, coeffs=$coeffs, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
