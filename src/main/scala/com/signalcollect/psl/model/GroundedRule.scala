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

package com.signalcollect.psl.model

import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.admm.optimizers.HingeLossOptimizer
import com.signalcollect.admm.optimizers.LinearConstraintOptimizer
import com.signalcollect.admm.optimizers.LinearLossOptimizer
import com.signalcollect.admm.optimizers.SquaredHingeLossOptimizer
import com.signalcollect.admm.optimizers.SquaredLossOptimizer
import com.signalcollect.psl.Optimizer
import com.signalcollect.util.Verifier

case class GroundedRule(
  id: Int, // TODO put a proper number
  definition: Rule,
  body: List[GroundedPredicate],
  head: List[GroundedPredicate]) extends GroundedRuleOrConstraint {

  /**
   * Constant is used in objective term:
   * weight * max(coeffs^T * x - constant, 0)
   * Can be computed at creation using the closed terms and doesn't ever change.
   *
   * Example: P1 and P2 and P3 => Q1 or Q2 or Q3
   * Translation: not (P1 and P2 and P3) or Q1 or Q2 or Q3
   * Distance to satisfaction - negate and use Lukasiewicz logic:
   * not (not (P1 and P2 and P3)) and not Q1 and not Q2 and not Q3
   * max {0, P1 + P2 -1 + P3 -1 + (1-Q1-1) + (1-Q2-1) + (1-Q3-1)}=
   * max {0, P1 + P2 + P3 -(size of body-1) - Q1 - Q2 - Q3}
   * if P1 = 0.6 and Q1 = 0.3
   * max {0, 0.6 + P2 + P3 -2 - 0.3 - Q2 - Q3} =
   * max {0, P2 + P3 - Q2 - Q3 -(2 - 0.6 + 0.3)}
   */
  def computeConstant: Double = {
    val contributionOfBody = body.zipWithIndex.map {
      case (b, i) =>
        if (definition.body(i).negated) { 1 - b.truthValue.getOrElse(0.0) }
        else { b.truthValue.getOrElse(0.0) }
    }
    val contributionOfHead = head.zipWithIndex.map {
      case (h, i) =>
        if (definition.head(i).negated) { 1 - h.truthValue.getOrElse(0.0) }
        else { h.truthValue.getOrElse(0.0) }
    }
    body.size - 1 - contributionOfBody.sum + contributionOfHead.sum
  }

  def computeCoefficientMatrix: Array[Double] = {
    val contributionOfBody = body.zipWithIndex.map {
      case (b, i) =>
        if (b.truthValue.isDefined) { 0.0 } // will be removed.
        else {
          if (definition.body(i).negated) { -1.0 }
          else { 1.0 }
        }
    }
    val contributionOfHead = head.zipWithIndex.map {
      case (h, i) =>
        if (h.truthValue.isDefined) { 0.0 } // will be removed.
        else {
          if (definition.head(i).negated) { 1.0 }
          else { -1.0 }
        }
    }

    val filtered = contributionOfHead.filter(_ != 0.0) ::: contributionOfBody.filter(_ != 0.0)
    filtered.toArray
  }

  def unboundGroundedPredicates: List[GroundedPredicate] = {
    // we add only the ones that have an unbound truth value, the others are taken care in the constant.
    head.filter(!_.truthValue.isDefined) ::: body.filter(!_.truthValue.isDefined)
  }

  def createOptimizableFunction(stepSize: Double, tolerance: Double = 0.0, breezeOptimizer: Boolean = false): Option[OptimizableFunction] = {
    // Easy optimization, if all are facts, ignore.
    if (unboundGroundedPredicates.size == 0) {
      return None
    }

    val constant = computeConstant
    val coefficientMatrix = computeCoefficientMatrix

    /**
     * Optimization: Check for trivially always true (distance to satisfaction always zero) formulas:
     * This can be done by assuming the variables with positive coefficient are all 1.0 (the max possible value) and
     * the ones with negative coefficient are all 0 (the min possible value).
     * If the value in this case is below 0, it will never be above 0, so we can safely ignore it.
     */
    val bestPossibleScenario = coefficientMatrix.map { coeff => if (coeff > 0) coeff else 0 }.sum - constant
    if (bestPossibleScenario < 0) {
      return None
    }

    // TODO: Define zMap - currently just initialized to 0.
    val zMap: Map[Int, Double] = unboundGroundedPredicates.map(gp => (gp.id, 0.0)).toMap
    val zIndices: Array[Int] = unboundGroundedPredicates.map(gp => gp.id).toArray

    if (definition.weight != Double.MaxValue) {
      // Not a hard rule.
      /**
       * Optimization: Check for functions that don't need the hingeloss, because they are always above 0.
       * This can be done by assuming the worst case scenario in which positive coefficient variables are all 0,
       * and the negative ones are always 1.
       * If the result is always positive, it will never be negative and doesn't
       * require a hingeloss but just a loss function.
       */
      val worstPossibleScenario = coefficientMatrix.map { coeff => if (coeff > 0) 0 else coeff }.sum - constant

      val optimizableFunction: OptimizableFunction = definition.distanceMeasure match {
        case Linear =>
          if (worstPossibleScenario > 0) {
            // The constant doesn't influence the minimization.
            if (breezeOptimizer) {
              new LinearLossOptimizer(
                id,
                weight = definition.weight,
                constant = constant,
                zIndices = zIndices,
                stepSize = stepSize,
                initialZmap = zMap,
                coefficientMatrix = coefficientMatrix)
            } else {
              Optimizer.linearLoss(stepSize, zMap, definition.weight, coefficientMatrix, zIndices, id)
            }

          } else {
            if (definition.weight < 0) {
              println(s"[WARNING]: Adding a concave function like: neg * max(0, coeff*x - const)")
            }
            if (breezeOptimizer) {
              new HingeLossOptimizer(
                id,
                weight = definition.weight,
                constant = constant,
                zIndices = zIndices,
                stepSize = stepSize,
                initialZmap = zMap,
                coefficientMatrix = coefficientMatrix)
            } else {
              Optimizer.hingeLoss(stepSize, zMap, definition.weight, constant, coefficientMatrix, zIndices, id)
            }
          }

        case Squared =>
          if (worstPossibleScenario > 0) {
            if (definition.weight < 0) {
              println(s"[WARNING]: Adding a concave function like: neg * (coeff*x - const)^2")
            }
            if (breezeOptimizer) {
              new SquaredLossOptimizer(
                id,
                weight = definition.weight,
                constant = constant,
                zIndices = zIndices,
                stepSize = stepSize,
                initialZmap = zMap,
                coefficientMatrix = coefficientMatrix)
            } else {
              Optimizer.squaredLinearLoss(stepSize, zMap, definition.weight, constant, coefficientMatrix, zIndices, id)
            }
          } else {
            if (definition.weight < 0) {
              println(s"[WARNING]: Adding a concave function like: neg * max(0, coeff*x - const)^2")
            }
            if (breezeOptimizer) {
              new SquaredHingeLossOptimizer(
                id,
                weight = definition.weight,
                constant = constant,
                zIndices = zIndices,
                stepSize = stepSize,
                initialZmap = zMap,
                coefficientMatrix = coefficientMatrix)
            } else {
              Optimizer.squaredHingeLoss(stepSize, zMap, definition.weight, constant, coefficientMatrix, zIndices, id)
            }
          }
        case _ =>
          throw new Exception("No distance measure specified.")
        //Optimizer.hingeLoss(stepSize, zMap, definition.weight, constant, coefficientMatrix, zIndices, id)
      }
      Some(optimizableFunction)
    } else {
      // A hard rule.
      // This would have been ~Infinity * max(0, coeff*x - constant)
      // We can rewrite this by adding a constraint: coeff*x - constant <= 0, or coeff*x <= constant
      val optimizableFunction: OptimizableFunction =
        if (breezeOptimizer) {
          new LinearConstraintOptimizer(id, "leq", constant, zIndices, stepSize, zMap, coefficientMatrix, tolerance)
        } else {
          Optimizer.linearConstraint(stepSize, zMap, "leq", constant, coefficientMatrix, zIndices, tolerance, id)
        }
      Some(optimizableFunction)
    }
  }
}
