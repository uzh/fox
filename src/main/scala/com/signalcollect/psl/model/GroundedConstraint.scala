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
import com.signalcollect.psl.Optimizer
import com.signalcollect.admm.optimizers.LinearConstraintOptimizer

/**
 * Constraints are in the form:
 * coeffs^T * x [?] constant
 * where [?] is ==, >=, or <=
 */

trait GroundedRuleOrConstraint {
  def isGroundedConstraint: Boolean = {
    this match {
      case v: GroundedConstraint => true
      case i: GroundedRule => false
    }
  }
  def isGroundedRule: Boolean = {
    this match {
      case i: GroundedRule => true
      case v: GroundedConstraint => false
    }
  }
}

case class GroundedConstraint(
  id: Int,
  ruleId: Int,
  property: PredicateProperty,
  groundedPredicates: List[GroundedPredicate],
  explicitCoeff: Array[Double] = Array(),
  explicitConst: Double = 0) extends GroundedRuleOrConstraint {

  def computeConstant: Double = {
    val sumOfFacts = groundedPredicates.flatMap(g => g.truthValue).sum
    property match {
      case PartialFunctional | InversePartialFunctional | Functional | InverseFunctional => 1.0 - sumOfFacts
      case Symmetric => sumOfFacts
      case LessOrEqual | GreaterOrEqual => explicitConst
      case _ => 1.0 - sumOfFacts
    }
  }

  def computeCoefficientMatrix: Array[Double] = {
    property match {
      case PartialFunctional | InversePartialFunctional | Functional | InverseFunctional =>
        unboundGroundedPredicates.map(_ => 1.0).toArray
      case Symmetric =>
        if (unboundGroundedPredicates.size == 2) { Array(1.0, -1.0) }
        else { Array[Double](1.0) }
      case LessOrEqual | GreaterOrEqual => explicitCoeff
      case _ => unboundGroundedPredicates.map(_ => 1.0).toArray
    }
  }

  def computeComparator = {
    property match {
      case PartialFunctional | InversePartialFunctional => "leq"
      case Functional | InverseFunctional | Symmetric => "eq"
      case LessOrEqual => "leq"
      case GreaterOrEqual => "geq"
      case _ => "leq"
    }
  }

  def unboundGroundedPredicates: List[GroundedPredicate] = {
    // we add only the ones that have an unbound truth value, the others are taken care in the constant.
    groundedPredicates.filter(!_.truthValue.isDefined)
  }

  def createOptimizableFunction(stepSize: Double, tolerance: Double = 0.0, breezeOptimizer: Boolean = false): Option[OptimizableFunction] = {
    // Easy optimization, if all are facts, ignore.
    if (unboundGroundedPredicates.size == 0)
      return None

    // Constraints work only for binary predicates.
    // Arity 0 is allowed only as retrocompatibility with grounded rules in PSL format.
    if (groundedPredicates(0).definition.arity != 0 && groundedPredicates(0).definition.arity != 2) {
      return None
    }

    val coefficientMatrix = computeCoefficientMatrix
    val constant = computeConstant
    val comparator = computeComparator

    /**
     * Optimization: Check for trivially always false constraints (they will be discarded)
     * Two cases:
     * - if the maximum possible value the predicates can achieve is always less than
     * greater or equal (geq) constraint.
     * - if the minimum possible value the predicates can achieve is always more than
     * less or equal (leq) constraint.
     */
    if (comparator != "leq") {
      val maxPossibleValue = coefficientMatrix.map { coeff => if (coeff > 0) coeff else 0 }.sum
      if (maxPossibleValue < constant) {
        //println(s"[Warning]: Constraint is always false and be discarded: ${property} on ${groundedPredicates}")
        //println(s"MaxPossibleValue: ${maxPossibleValue}, constant: ${constant}")
        return None
      }
    }
    if (comparator != "geq") {
      val minPossibleValue = coefficientMatrix.map { coeff => if (coeff > 0) 0 else coeff }.sum
      if (minPossibleValue > constant) {
        //println(s"[Warning]: Constraint is always false and will be discarded: ${property} on ${groundedPredicates}")
        //println(s"MinimumPossibleValue: ${minPossibleValue}, constant: ${constant}")
        return None
      }
    }

    // TODO: Define zMap - currently just initialized to 0.
    val zMap: Map[Int, Double] = unboundGroundedPredicates.map(gp => (gp.id, 0.0)).toMap
    val zIndices: Array[Int] = unboundGroundedPredicates.map(gp => gp.id).toArray
    val optimizableFunction: OptimizableFunction =
      if (breezeOptimizer) {
        new LinearConstraintOptimizer(id, comparator, constant, zIndices, stepSize, zMap, coefficientMatrix, tolerance)
      } else {
        Optimizer.linearConstraint(stepSize, zMap, comparator, constant, coefficientMatrix, zIndices, tolerance, id)
      }
    Some(optimizableFunction)
  }

}
