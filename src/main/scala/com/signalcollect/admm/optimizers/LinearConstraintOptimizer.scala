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

class LinearConstraintOptimizer(
  setId: Int,
  val comparator: String,
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

  def basicFunction = {
    new Function1[DenseVector[Double], Double] {
      def apply(x: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(x)
        if (comparator == "leq" && coeffsDotX <= constant
          || comparator == "geq" && coeffsDotX >= constant
          || comparator == "eq" && coeffsDotX == constant) {
          0.0
        } else {
          Double.MaxValue
        }
      }
    }
  }

  def evaluateAtEfficient(someX: Array[Double]): Double = {
    basicFunction(DenseVector(someX))
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

  val length: Double = {
    val sumOfSquaredCoefficients = coefficientMatrix.map(v => v*v).sum
    math.sqrt(sumOfSquaredCoefficients)
  }
  
  val unitNormalVector: DenseVector[Double] = {
    val unitNormal = coefficientMatrix.map(v=> v/length)
    DenseVector(unitNormal)
  }
  
  /**
   * Adaptation of Stephen Bach's solver.
   *
   * Objective of the form:
   * 0 if coeffs^T * x CMP constant,
   * infinity otherwise,
   * where CMP is ==, >=, or <=
   * All coeffs must be non-zero.
   */
  def optimizeEfficient(
    consensusAssignments: Array[Double]) {
    setZ(consensusAssignments)
    val newXIfNoLoss = z - (y / stepSize)
    val total = coeffs.dot(newXIfNoLoss)
    x = newXIfNoLoss
    if ((comparator == "leq" && total <= constant)
      || (comparator == "geq" && total >= constant)
      || (comparator == "eq" && total == constant)) {
      return
    } else {
      if (x.length == 1){
        x(0) = constant / coeffs(0)
        return
      }
      // Project x onto coeffsDotX == constant plane.
      var distance = - constant/length
      distance += x.dot(unitNormalVector)
      x = x - (unitNormalVector * distance)
    }
  }

  override def toString = s"LinearConstraintOptimizer(x=$x, y=$y, z=$z, coeffs=$coeffs, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
