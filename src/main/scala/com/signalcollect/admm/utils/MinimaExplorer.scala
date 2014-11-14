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

package com.signalcollect.admm.utils;

import java.io.File
import com.signalcollect.util.IntDoubleHashMap
import com.signalcollect.psl.model.GroundedConstraint
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.admm.Wolf
import com.signalcollect.psl.Grounding
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.admm.optimizers.ConvexConstraintOptimizer
import com.signalcollect.admm.optimizers.LinearLossOptimizer
import com.signalcollect.admm.optimizers.LinearConstraintOptimizer
import com.signalcollect.admm.optimizers.HingeLossOptimizer
import com.signalcollect.admm.optimizers.SquaredHingeLossOptimizer
import com.signalcollect.admm.optimizers.OptimizerBase

/**
 * Explores the multiple minima based on one solution.
 * At the moment as a proof of concept changes one variable at a time.
 */
object MinimaExplorer {

  def exploreFromString(example: String, config: InferencerConfig): List[(String, Double, Double)] = {
    val pslData = PslParser.parse(example)
    // This is the same as the inferencer, we copy it so we don't have to recreate the functions.
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData, config.isBounded, config.removeSymmetricConstraints)
    val functions = groundedRules.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    val constraints = groundedConstraints.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    val functionsAndConstraints = functions ++ constraints
    println(s"Problem converted to consensus optimization with ${functions.size} functions and ${constraints.size} constraints that are not trivially true.")

    val solution = Wolf.solveProblem(
      functionsAndConstraints,
      None,
      config.getWolfConfig)

    val objectiveFunctionVal: Double = functionsAndConstraints.foldLeft(0.0) {
      case (sum, nextFunction) => sum + nextFunction.evaluateAt(solution.results)
    }

    println(s"First inference completed, objective value: $objectiveFunctionVal")

    val optimizerBaseFunctions = functionsAndConstraints.flatMap {
      f =>
        f match {
          case h: HingeLossOptimizer => Some(h)
          case s: SquaredHingeLossOptimizer => Some(s)
          case _ => None
        }
    }

    val hardFunctions = functionsAndConstraints.flatMap {
      f =>
        f match {
          case l: LinearConstraintOptimizer => Some(l)
          case _ => None
        }
    }

    val totalZindices = optimizerBaseFunctions.flatMap(_.zIndices).toSet.toArray

    var id = functions.size + constraints.size

    // Keep the objective function with the objective function val as a hard constraint.
    val newConstraint = new ConvexConstraintOptimizer(
      id,
      objectiveFunctionVal,
      totalZindices,
      config.stepSize,
      totalZindices.map(t => (t, 0.0)).toMap,
      optimizerBaseFunctions,
      config.tolerance)

    val newConstraints = hardFunctions ++ List(newConstraint)

    // For each grounded predicate create two problems, one with the min x and one with the min -x
    val result = idToGpMap.map {
      case (zIndex, gp) =>
        val minNewFunction = new LinearLossOptimizer(
          { id = id + 1; id },
          1.0,
          0.0,
          Array(zIndex),
          config.stepSize,
          Map(zIndex -> 0.0),
          Array(1.0))

        val minSolution = Wolf.solveProblem(
          newConstraints ++ List(minNewFunction),
          None,
          config.getWolfConfig).results.get(zIndex)

        val maxNewFunction = new LinearLossOptimizer(
          { id = id + 1; id },
          1.0,
          0.0,
          Array(zIndex),
          config.stepSize,
          Map(zIndex -> 0.0),
          Array(-1.0))

        val maxSolution = Wolf.solveProblem(
          newConstraints ++ List(maxNewFunction),
          None,
          config.getWolfConfig).results.get(zIndex)

        val minValue = math.min(minSolution, maxSolution)
        val maxValue = math.max(minSolution, maxSolution)
        (gp.toString, minValue, maxValue)
    }
    result.toList
  }
}