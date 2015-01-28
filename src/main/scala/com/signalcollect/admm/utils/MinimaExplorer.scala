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
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.admm.optimizers.ConvexConstraintOptimizer
import com.signalcollect.admm.optimizers.LinearLossOptimizer
import com.signalcollect.admm.optimizers.LinearConstraintOptimizer
import com.signalcollect.admm.optimizers.HingeLossOptimizer
import com.signalcollect.admm.optimizers.SquaredHingeLossOptimizer
import com.signalcollect.admm.optimizers.SquaredLossOptimizer
import com.signalcollect.admm.optimizers.OptimizerBase
import com.signalcollect.admm.Wolf
import com.signalcollect.psl.Grounding
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.model.GroundedConstraint
import com.signalcollect.psl.model.GroundedRule

/**
 * Explores the multiple minima based on one solution.
 * At the moment as a proof of concept changes one variable at a time.
 */
object MinimaExplorer {

  def approxUpperBound = 0.9
  def approxLowerBound = 0.1

  def roundUpDouble(value: Double, exponent: Int = 3) = {
    ((math.pow(10, exponent) * value).round / math.pow(10, exponent).toDouble)
  }

  def roundUpWithCutoff(value: Double, exponent: Int = 3) = {
    if (value >= approxUpperBound) { 1.0 } else if (value <= approxLowerBound) { 0.0 } else { roundUpDouble(value, exponent) }
  }

  def divideFunctionsAndConstraints(functionsAndConstraints: List[OptimizableFunction]) = {
    val optimizerBaseFunctions = functionsAndConstraints.flatMap {
      f =>
        f match {
          case h: HingeLossOptimizer => Some(h)
          case s: SquaredHingeLossOptimizer => Some(s)
          case l: LinearLossOptimizer => Some(l)
          case q: SquaredLossOptimizer => Some(q)
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
    //println(s"Number of optimizer base functions: ${optimizerBaseFunctions.size}, number of constraints: ${hardFunctions.size}")
    (optimizerBaseFunctions, hardFunctions)
  }

  def exploreFromString(example: String, config: InferencerConfig = InferencerConfig(),
    groundedPredicateNames: List[String] = List.empty): List[(String, Double, Double, Double)] = {
    val pslData = PslParser.parse(example)
    runExploration(pslData, config, groundedPredicateNames)
  }

  def exploreFromFile(example: File, config: InferencerConfig = InferencerConfig(),
    groundedPredicateNames: List[String] = List.empty): List[(String, Double, Double, Double)] = {
    val pslData = PslParser.parse(example)
    runExploration(pslData, config, groundedPredicateNames)
  }

  def runExploration(pslData: ParsedPslFile, config: InferencerConfig = InferencerConfig(),
    groundedPredicateNames: List[String] = List.empty): List[(String, Double, Double, Double)] = {
    // This is the same as the inferencer, we copy it so we don't have to recreate the functions.
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData, config)
    println(s"Grounding completed: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")
    //    idToGpMap.map(gp => println(s"${gp._2} ${gp._2.truthValue}"))
    //    groundedRules.map(println(_))
    val functions = groundedRules.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    val constraints = groundedConstraints.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    //val (optimizerBaseFunctions, hardFunctions) = divideFunctionsAndConstraints(functions ++ constraints)
    val functionsAndConstraints = functions ++ constraints
    val solution = Wolf.solveProblem(
      functionsAndConstraints,
      None,
      config.getWolfConfig)
    val objectiveFunctionVal: Double = functionsAndConstraints.foldLeft(0.0) {
      case (sum, nextFunction) => {
        val nextResult = nextFunction.evaluateAt(solution.results)
        if (nextResult == Double.MaxValue) {
          val gr = groundedRules.filter(_.id == nextFunction.id.getOrElse(0)).headOption match {
            case Some(grs) => grs.toString
            case None => ""
          }
          val gc = groundedConstraints.filter(_.id == nextFunction.id.getOrElse(0)).headOption match {
            case Some(gcs) => gcs.toString
            case None => ""
          }
          println(s"Constraint broken: $gr $gc")
        }
        sum + nextResult
      }
    }
    println(s"First inference completed, objective value: $objectiveFunctionVal")

    val groundedPredicatesToTest = if (groundedPredicateNames.isEmpty) {
      idToGpMap.filter(!_._2.truthValue.isDefined)
    } else {
      idToGpMap.filter(!_._2.truthValue.isDefined) filter (gp => groundedPredicateNames.exists(gp._2.toString.contains(_)))
    }

    val naivePredicateBounds = NaiveMinimaExplorer.exploreResultsNaive(groundedRules, groundedConstraints, solution.results)

    // For each grounded predicate create two problems, one with the min x and one with the min -x
    val result = groundedPredicatesToTest.map {
      case (zIndex, gp) =>
        val (naivePredicateMinBound, naivePredicateMaxBound) = naivePredicateBounds.getOrElse(zIndex, (Double.MaxValue, Double.MinValue))
        val minValue = if (naivePredicateMinBound <= approxLowerBound) {
          0.0
        } else {
          val functionsNew = groundedRules.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
          val constraintsNew = groundedConstraints.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
          val (optimizerBaseFunctions, hardFunctions) = divideFunctionsAndConstraints(functionsNew ++ constraintsNew)
          val totalZindices = optimizerBaseFunctions.flatMap(_.zIndices).toSet.toArray

          val newConstraint = {
            if (optimizerBaseFunctions.size > 0) {
              // Keep the objective function with the objective function val as a hard constraint.
              List(new ConvexConstraintOptimizer(
                groundedRules.size + groundedConstraints.size + 2,
                objectiveFunctionVal,
                totalZindices,
                config.stepSize,
                totalZindices.map(t => (t, 0.0)).toMap,
                optimizerBaseFunctions,
                0))
            } else { List.empty }
          }
          //println(s"newConstraint $newConstraint")

          val minNewFunction = new SquaredHingeLossOptimizer(
            groundedRules.size + groundedConstraints.size + 3,
            100.0,
            0.0,
            Array(zIndex),
            config.stepSize,
            Map(zIndex -> 0.0),
            Array(1.0))

          println(s"minSolution $gp")
          val minSolution = Wolf.solveProblem(
            hardFunctions ++ newConstraint ++ List(minNewFunction),
            None,
            config.getWolfConfig)

          minSolution.results.get(zIndex)

        }
        println(s"maxSolution $gp")
        val maxValue = if (naivePredicateMaxBound >= approxUpperBound) {
          1.0
        } else {
          val functionsNew = groundedRules.flatMap(_.createOptimizableFunction(config.stepSize, 0.0, config.breezeOptimizer))
          val constraintsNew = groundedConstraints.flatMap(_.createOptimizableFunction(config.stepSize, 0.0, config.breezeOptimizer))
          val (optimizerBaseFunctions, hardFunctions) = divideFunctionsAndConstraints(functionsNew ++ constraintsNew)
          val totalZindices = optimizerBaseFunctions.flatMap(_.zIndices).toSet.toArray

          val newConstraint = {
            if (optimizerBaseFunctions.size > 0) {
              // Keep the objective function with the objective function val as a hard constraint.
              List(new ConvexConstraintOptimizer(
                groundedRules.size + groundedConstraints.size + 2,
                objectiveFunctionVal,
                totalZindices,
                config.stepSize,
                totalZindices.map(t => (t, 0.0)).toMap,
                optimizerBaseFunctions,
                0))
            } else { List.empty }
          }

          val maxNewFunction = new SquaredHingeLossOptimizer(
            groundedRules.size + groundedConstraints.size + 3,
            100.0,
            -1.0,
            Array(zIndex),
            config.stepSize,
            Map(zIndex -> 0.0),
            Array(-1.0))

          val maxSolution = Wolf.solveProblem(
            hardFunctions ++ newConstraint ++ List(maxNewFunction),
            None,
            config.getWolfConfig)
          maxSolution.results.get(zIndex)
        }

        val boundedMaxValue = roundUpWithCutoff(math.max(naivePredicateMaxBound, maxValue))
        val boundedMinValue = roundUpWithCutoff(math.min(naivePredicateMinBound, minValue))
        val boundedMiddleValue = roundUpWithCutoff(solution.results.get(zIndex))
        assert(boundedMinValue <= boundedMaxValue)
        assert(boundedMinValue <= boundedMiddleValue)
        assert(boundedMiddleValue <= boundedMaxValue)

        (gp.toString, boundedMiddleValue, boundedMinValue, boundedMaxValue)
    }
    result.toList
  }
}

object NaiveMinimaExplorer {
  def evaluateSingleFunction(function: Option[OptimizableFunction], variableBindings: Map[Int, Double],
    perturbedVariableBindings: Map[Int, Double] = Map.empty): Double = {
    function match {
      case Some(nextF) =>
        val variables = nextF.idToIndexMappings
        val x = variables.map {
          v =>
            perturbedVariableBindings.get(v).getOrElse(variableBindings(v))
        }
        nextF.evaluateAtEfficient(x)
      case None => 0.0
    }
  }

  def evaluatePerturbation(groundedPredicatesToFunctions: Map[Int, (List[GroundedRule], Double)],
    groundedPredicatesToConstraints: Map[Int, (List[GroundedConstraint], Double)],
    variableBindings: Map[Int, Double], perturbedValue: Double): Set[Int] = {
    val candidateVariablesForFunctions = groundedPredicatesToFunctions.flatMap {
      case (v, (listOfGroundedRules, totalContribution)) =>
        if (variableBindings(v) != perturbedValue) {
          // For that int change to 0.
          val newContribution = listOfGroundedRules.foldLeft(0.0) {
            case (aggr, nextRule) =>
              val perturbedVariableBindings = Map(v -> perturbedValue)
              aggr + evaluateSingleFunction(nextRule.createOptimizableFunction(1.0), variableBindings, perturbedVariableBindings)
          }
          if (newContribution <= totalContribution) {
            Some(v)
          } else {
            None
          }
        } else { None }
    }.toSet

    val candidateVariablesForConstraints = groundedPredicatesToConstraints.flatMap {
      case (v, (listOfGroundedConstraints, totalViolation)) =>
        if (variableBindings(v) != perturbedValue) {
          // For that int change to 0.
          val newViolation = listOfGroundedConstraints.foldLeft(0.0) {
            case (aggr, nextConstraint) =>
              val perturbedVariableBindings = Map(v -> perturbedValue)
              aggr + evaluateSingleFunction(nextConstraint.createOptimizableFunction(1.0, -1), variableBindings, perturbedVariableBindings)
          }
          if (newViolation <= totalViolation) {
            Some(v)
          } else {
            None
          }
        } else { None }
    }.toSet
    val results = candidateVariablesForFunctions.intersect(candidateVariablesForConstraints)
    //println("Candidate variables to change to value: " + perturbedValue + " : " + results)
    results
  }

  def evaluatePerturbationRecursively(groundedPredicatesToFunctions: Map[Int, (List[GroundedRule], Double)],
    groundedPredicatesToConstraints: Map[Int, (List[GroundedConstraint], Double)],
    variableBindings: Map[Int, Double], perturbedValue: Double, accumulatedIndexes: Set[Int] = Set.empty): Set[Int] = {
    val candidateIndexes = evaluatePerturbation(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, perturbedValue)
    if (candidateIndexes.size == 0) {
      accumulatedIndexes
    } else {
      val perturbedBindings = candidateIndexes.map(i => (i, perturbedValue))
      evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints,
        variableBindings ++ perturbedBindings, perturbedValue, accumulatedIndexes ++ candidateIndexes)
    }
  }

  def exploreResultsNaive(groundedRules: List[GroundedRule], groundedConstraints: List[GroundedConstraint],
    varBindings: IntDoubleHashMap) = {
    // Create a map for each predicate with the related functions, constraints and their value at the given point.
    // TODO: this is the most naive way possible, optimize.
    val variableBindings = varBindings.toScalaMap

    var groundedPredicatesToFunctions = Map[Int, (List[GroundedRule], Double)]()
    var groundedPredicatesToConstraints = Map[Int, (List[GroundedConstraint], Double)]()

    groundedRules.foreach {
      groundedRule =>
        val contributionOfGroundedRule = evaluateSingleFunction(groundedRule.createOptimizableFunction(1.0),
          variableBindings)
        groundedRule.variables.map {
          v =>
            groundedPredicatesToFunctions.get(v) match {
              case Some((listOfGroundedRules, totalContribution)) =>
                groundedPredicatesToFunctions += (v -> (listOfGroundedRules ++ List(groundedRule), totalContribution + contributionOfGroundedRule))
              case None =>
                groundedPredicatesToFunctions += (v -> (List(groundedRule), contributionOfGroundedRule))
            }
        }
    }

    groundedConstraints.foreach {
      groundedConstraint =>
        val violationOfGroundedConstraint = evaluateSingleFunction(groundedConstraint.createOptimizableFunction(1.0, -1),
          variableBindings)
        groundedConstraint.variables.map {
          v =>
            groundedPredicatesToConstraints.get(v) match {
              case Some((listOfGroundedConstraints, totalViolation)) =>
                groundedPredicatesToConstraints += (v -> (listOfGroundedConstraints ++ List(groundedConstraint), totalViolation + violationOfGroundedConstraint))
              case None =>
                groundedPredicatesToConstraints += (v -> (List(groundedConstraint), violationOfGroundedConstraint))

            }
        }
    }

    // Perturbate the evaluation of the functions, one variable at a time.
    // Keep only variables that don't break the constraints more and don't increase the value of the function.
    // Most naive way possible, check only 0 and 1.
    // TODO: proper bounds.
    val bounds0 = evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 0) ++
      evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 0.01) ++
      evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 0.1)
    val bounds1 = evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 1) ++
      evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 0.99) ++
      evaluatePerturbationRecursively(groundedPredicatesToFunctions, groundedPredicatesToConstraints, variableBindings, 0.9)

    val bounds0and1 = bounds0.intersect(bounds1)
    val bounds0only = bounds0.diff(bounds1)
    val bounds1only = bounds1.diff(bounds0)

    val results = bounds0and1.flatMap { i => Map(i -> (0.0, 1.0)) }.toMap ++
      bounds0only.flatMap { i => Map(i -> (0.0, variableBindings(i))) }.toMap ++
      bounds1only.flatMap { i => Map(i -> (variableBindings(i), 1.0)) }.toMap

    results
  }
}