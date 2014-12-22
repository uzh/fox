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
  constant: Double,
  zIndices: Array[Int],
  stepSize: Double = 1.0,
  initialZmap: Map[Int, Double],
  coefficientMatrix: Array[Double],
  val tolerance: Double = 0.0) extends OptimizerBase(setId, constant, zIndices, stepSize, initialZmap, coefficientMatrix) {


  def basicFunction = {
    new Function1[DenseVector[Double], Double] {
      def apply(d: DenseVector[Double]) = {
        val coeffsDotX = coeffs.dot(d)
        if (comparator == "leq" && coeffsDotX > constant
          || comparator == "geq" && coeffsDotX < constant
          || comparator == "eq" && coeffsDotX != constant) {
          // If the constraint is broken, check how much.
          val absDiff = math.abs(coeffsDotX - constant)
          // Under a certain tolerance, ignore the violation.
          if (tolerance >= 0 && absDiff <= tolerance) {
            0.0
          } else {
            Double.MaxValue
          }
        } else {
          0.0
        }
      }
    }
  }

  def evaluateAtEfficient(someX: Array[Double]): Double = {
    basicFunction(DenseVector(someX))
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
    if ((comparator == "leq" && total > constant)
      || (comparator == "geq" && total < constant)
      || (comparator == "eq" && total != constant)) {
      // If the constraint is broken, check how much.
      //      val absDiff = math.abs(total - constant)
      //      // Under a certain tolerance, ignore the violation.
      //      if (tolerance >= 0 && absDiff <= tolerance) {
      //        return
      //      }

      if (x.length == 1) {
        x(0) = constant / coeffs(0)
        return
      }
      // Project x onto coeffsDotX == constant plane.
      var distance = -constant / length
      distance += x.dot(unitNormalVector)
      x = x - (unitNormalVector * distance)
    }
  }

  override def toString = s"LinearConstraintOptimizer(x=$x, y=$y, z=$z, coeffs=$coeffs, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
