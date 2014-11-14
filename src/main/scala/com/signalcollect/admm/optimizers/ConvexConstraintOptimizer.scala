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

// A sum of optimizer base constrained by a leq to a constant.
class ConvexConstraintOptimizer(
  setId: Int,
  constant: Double,
  zIndices: Array[Int],
  stepSize: Double = 1.0,
  initialZmap: Map[Int, Double],
  basicFunctions: List[OptimizerBase],
  val tolerance: Double = 0.0) extends OptimizerBase(setId, constant, zIndices, stepSize, initialZmap, Array(0)) {

  val zIndicesPerFunction = basicFunctions.flatMap { f =>
    val zIndicesF = zIndices.zipWithIndex.flatMap { idIndex => if (f.zIndices.toList.contains(idIndex._1)) { Some(idIndex._2) } else { None } }
    Map(f.id -> zIndicesF)
  }.toMap

  def basicFunction = {
    new Function1[DenseVector[Double], Double] {
      def apply(x: DenseVector[Double]) = {
        val total = basicFunctions.map { f =>
          val xInF = zIndicesPerFunction.get(f.id).get.map(x.valueAt(_))
          f.evaluateAtEfficient(xInF)
        }.sum
        if (total > constant) {
          // If the constraint is broken, check how much.
          val absDiff = math.abs(total - constant)
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
   * Try a naive alternating projection.
   */
  def optimizeEfficient(
    consensusAssignments: Array[Double]) {
    setZ(consensusAssignments)
    val newXIfNoLoss = z - (y / stepSize)
    val total = basicFunctions.map { f =>
      val xInF = zIndicesPerFunction.get(f.id).get.map(newXIfNoLoss.valueAt(_))
      f.evaluateAtEfficient(xInF)
    }.sum
    x = newXIfNoLoss
    if (total > constant) {

      // Project x onto total <= constant region.
      // Try to do successive projections (naive alternating projection).
      basicFunctions.map { f =>
        val xInF = DenseVector(zIndicesPerFunction.get(f.id).get.map(x.valueAt(_)))
        var distance = -constant / f.length
        distance += xInF.dot(f.unitNormalVector)
        val newX = xInF - (f.unitNormalVector * distance)
        zIndicesPerFunction.get(f.id).get.zipWithIndex.map { case (i, j) => x.update(i, newX(j)) }
      }
    }
  }

  override def toString = s"ConvexConstraintOptimizer(x=$x, y=$y, z=$z, constant=$constant, zIndices=${zIndices.mkString("[", ",", "]")})"
}
