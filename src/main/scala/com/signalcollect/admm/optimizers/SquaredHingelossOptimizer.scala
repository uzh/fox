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

class SquaredHingeLossOptimizer(
  setId: Int,
  val weight: Double,
  val constant: Double,
  val zIndices: Array[Int],
  var stepSize: Double = 1.0,
  initialZmap: Map[Int, Double],
  coefficientMatrix: Array[Double]) extends OptimizableFunction {

  def id = Some(setId)

  def norm2WithoutSquareRoot(v: DenseVector[Double]): Double = {
    var squared = 0.0
    v.foreach(x => squared += x * x)
    squared
  }

  lazy val quadraticLossFunction = {
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        //weight * max(coeffs^T * x - constant, 0)^2 + stepSize/2 * norm2(x - z + (y / stepSize))^2
        (math.pow(math.max(coeffsDotX - constant, 0), 2) * weight + stepSize / 2 * norm2WithoutSquareRoot(x - z + y / stepSize),
          // gradient of function above.
          if (constant < coeffsDotX) {
            coeffs * (coeffsDotX - constant) * 2.0 * weight + (x - z) * stepSize + y
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
        //weight * max(coeffs^T * x - constant, 0)^2
        (math.pow(math.max(coeffsDotX - constant, 0), 2) * weight,
          // gradient of function above.
          if (constant < coeffsDotX) {
            // weight * [ c_0^2 *x_0^2  + 2*c_0*c_1*x_1*x_0 + ... + 2*c_0*c_n*x_n*x_0 - 2*c_0*constant * x_0...]
            // df/dx_0 = weight* 2* c_0* [c_0*x_0 + c_1*x_1 + ... + c_n*x_n - constant]
            coeffs * (coeffsDotX - constant) * 2.0 * weight
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

  var z: DenseVector[Double] = DenseVector(zIndices.map(initialZmap))
  var x: DenseVector[Double] = DenseVector.zeros(zIndices.length)
  var y: DenseVector[Double] = DenseVector.zeros(zIndices.length)

  val coeffs = DenseVector(coefficientMatrix: _*)

  def getStepSize: Double = stepSize
  def setStepSize(s: Double) = stepSize = s
  def getYEfficient: Array[Double] = y.data
  def getX = x.data
  def setY(y: Array[Double]) {
    this.y = DenseVector(y: _*)
  }
  def updateLagrangeEfficient(newZ: Array[Double]) {
    z = DenseVector(newZ)
    y += (x - z) * stepSize
  }
  def idToIndexMappings: Array[Int] = zIndices
  def setZ(newZ: Array[Double]) {
    z = DenseVector(newZ)
  }

  /**
   * Adaptation of Stephen Bach's solver.
   *
   * Objective term of the form
   * weight * [max(coeffs^T * x - constant, 0)]^2
   */
  def optimizeEfficient(
    consensusAssignments: Array[Double]) {
    setZ(consensusAssignments)
    val newXIfNoLoss = z - (y / stepSize)
    val total = coeffs.dot(newXIfNoLoss)
    if (total <= constant) {
      x = newXIfNoLoss
    } else {
      // Also consider quadratic loss:
      // argmin(weight * (coeffs^T * x - constant)^2 + stepSize/2 * norm2(x - z + (y / stepSize))^2)
      if (x.length == 1) {
        x = newXIfNoLoss
        val c0 = coeffs(0)
        x(0) += c0 * 2.0 * weight * constant
        x(0) /= 2 * weight * c0 * c0 + stepSize
      } else if (x.length == 2) {
        x = newXIfNoLoss
        /* Construct constant term in the gradient (moved to right-hand side) */
        val c0 = coeffs(0)
        val c1 = coeffs(1)
        val a0 = 2 * weight * c0 * c0 + stepSize
        val b1 = 2 * weight * c1 * c1 + stepSize
        val a1b0 = 2 * weight * c0 * c1
        x += coeffs * (2.0 * weight * constant)
        x(1) -= a1b0 * x(0) / a0
        x(1) /= b1 - a1b0 * a1b0 / a0
        x(0) -= a1b0 * x(1)
        x(0) /= a0
      } else {
        x = minimize(quadraticLossFunction, x)
      }
    }
  }

  override def toString = s"SquaredHingeLossOptimizer(x=$x, y=$y, z=$z, coeffs=$coeffs, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
