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

package com.signalcollect.psl

import com.signalcollect.admm.optimizers.OptimizableFunction

object Optimizer {

  /**
   * Scala wrapper for Stephen Bach's ADMM LinearConstraintTerm optimizer.
   * if coeffs^T * x [?] constant 0, otherwise infinity.
   * where [?] is ==, >=, or <=
   */
  def linearConstraint(
    stepSize: Double,
    zMap: Map[Int, Double],
    comparator: String,
    constant: Double,
    coefficientMatrix: Array[Double],
    zIndices: Array[Int],
    tolerance: Double,
    id: Int): OptimizableFunction = {
    val reasoner = new ADMMReasoner(stepSize, JavaConversionHelper.toZMapJava(zMap))
    val optimizer = new LinearConstraintTerm(
      reasoner,
      zIndices,
      coefficientMatrix,
      constant,
      comparator,
      tolerance)
    new PslOptimizerWrapper(stepSize, zIndices, optimizer, id)
  }

  /**
   * Scala wrapper for Stephen Bach's ADMM LinearLossTerm optimizer.
   * The LinearLossTerm optimizer is specialized in minimizing terms in the form:
   * weight * coeffs^T * x
   */
  def linearLoss(
    stepSize: Double,
    zMap: Map[Int, Double],
    weight: Double,
    coefficientMatrix: Array[Double],
    zIndices: Array[Int],
    id: Int): OptimizableFunction = {
    val reasoner = new ADMMReasoner(stepSize, JavaConversionHelper.toZMapJava(zMap))
    val optimizer = new LinearLossTerm(
      reasoner,
      zIndices,
      coefficientMatrix,
      weight)
    new PslOptimizerWrapper(stepSize, zIndices, optimizer, id)
  }

  /**
   * Scala wrapper for Stephen Bach's ADMM SquaredLinearLossTerm optimizer.
   * The SquaredLinearLossTerm optimizer is specialized in minimizing terms in the form:
   * weight * (coeffs^T * x - constant)^2
   */
  def squaredLinearLoss(
    stepSize: Double,
    zMap: Map[Int, Double],
    weight: Double,
    constant: Double,
    coefficientMatrix: Array[Double],
    zIndices: Array[Int],
    id: Int): OptimizableFunction = {
    val reasoner = new ADMMReasoner(stepSize, JavaConversionHelper.toZMapJava(zMap))
    val optimizer = new SquaredLinearLossTerm(
      reasoner,
      zIndices,
      coefficientMatrix,
      constant,
      weight)
    new PslOptimizerWrapper(stepSize, zIndices, optimizer, id)
  }

  /**
   * Scala wrapper for Stephen Bach's ADMM HingeLossTerm optimizer.
   * The HingeLossTerm optimizer is specialized in minimizing terms in the form:
   * weight * max(coeffs^T * x - constant, 0)
   * This represents the form in which we usually get PSL distances to satisfaction in case p = 1.
   */
  def hingeLoss(
    stepSize: Double,
    zMap: Map[Int, Double],
    weight: Double,
    constant: Double,
    coefficientMatrix: Array[Double],
    zIndices: Array[Int],
    id: Int): OptimizableFunction = {
    val reasoner = new ADMMReasoner(stepSize, JavaConversionHelper.toZMapJava(zMap))
    val optimizer = new HingeLossTerm(
      reasoner,
      zIndices,
      coefficientMatrix,
      constant,
      weight)
    new PslOptimizerWrapper(stepSize, zIndices, optimizer, id)
  }

  /**
   * Scala wrapper for Stephen Bach's ADMM SquaredHingeLossTerm optimizer.
   * The SquaredHingeLoss optimizer is specialized in minimizing terms in the form:
   * weight * max(coeffs^T * x - constant, 0)^2
   * This represents the form in which we usually get PSL distances to satisfaction in case p = 2.
   */
  def squaredHingeLoss(
    stepSize: Double,
    zMap: Map[Int, Double],
    weight: Double,
    constant: Double,
    coefficientMatrix: Array[Double],
    zIndices: Array[Int],
    id: Int): OptimizableFunction = {
    val reasoner = new ADMMReasoner(stepSize, JavaConversionHelper.toZMapJava(zMap))
    val optimizer = new SquaredHingeLossTerm(
      reasoner,
      zIndices,
      coefficientMatrix,
      constant,
      weight)
    new PslOptimizerWrapper(stepSize, zIndices, optimizer, id)
  }
}

class PslOptimizerWrapper(
  stepSize: Double,
  zIndices: Array[Int],
  val pslOptimizer: ADMMObjectiveTerm,
  setId: Int) extends OptimizableFunction with Serializable {

  def id = Some(setId)

  override def optimizeEfficient(z: Array[Double]) = {
    assert(idToIndexMappings.length == z.length,
      "zMap needs to have the same size as the one with which this optimizer was initized.\n" +
        s"previous: ${pslOptimizer.reasoner.z} new: ${z.mkString("[", ",", "]")}, idToIndexMap=${idToIndexMappings.mkString("[", ",", "]")}")
    setZ(z)
    pslOptimizer.minimize
  }

  override def setY(other: Array[Double]) = {
    for (i <- 0 until other.size) {
      pslOptimizer.y(i) = other(i)
    }
  }
  override def getYEfficient: Array[Double] = pslOptimizer.y

  override def updateLagrangeEfficient(z: Array[Double]) = {
    assert(idToIndexMappings.length == z.length,
      "z needs to have the same size as the one with which this optimizer was initized.\n" +
        s"previous: ${pslOptimizer.reasoner.z} new: ${z.mkString("[", ",", "]")}, idToIndexMap=${idToIndexMappings.mkString("[", ",", "]")}")
    setZ(z)
    pslOptimizer.updateLagrange
  }

  override def idToIndexMappings: Array[Int] = zIndices

  override def setStepSize(step: Double) = {
    pslOptimizer.setStepSize(JavaConversionHelper.toJDouble(step))
  }

  override def getStepSize: Double = pslOptimizer.reasoner.stepSize

  override def setZ(z: Array[Double]) {
    pslOptimizer.setZMap(JavaConversionHelper.toZMapJava(arrayToMap(z)))
  }
  override def evaluateAtEfficient(x: Array[Double]): Double = {
    pslOptimizer.evaluateAt(JavaConversionHelper.toZMapJava(arrayToMap(x)))
  }

  override def getX = pslOptimizer.x

  override def toString = pslOptimizer.toString

}

object JavaConversionHelper {
  // Convert zMap to a Java object.
  def toZMapJava(zMap: Map[Int, Double]): Map[java.lang.Integer, java.lang.Double] = zMap.map(tuple => (toJInt(tuple._1), toJDouble(tuple._2)))
  def toJInt(i: Int): java.lang.Integer = java.lang.Integer.valueOf(i)
  def toJDouble(d: Double): java.lang.Double = java.lang.Double.valueOf(d)

}