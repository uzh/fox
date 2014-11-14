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

import com.signalcollect.ExecutionInformation
import com.signalcollect.admm.Wolf
import com.signalcollect.admm.WolfConfig
import com.signalcollect.admm.ProblemSolution
import com.signalcollect.admm.utils.Timer
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.Fact
import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.Rule
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.GroundedConstraint
import java.io.File
import akka.actor.ActorRef
import com.signalcollect.psl.model.GroundedPredicate

/**
 * The main class for PSL inference.
 * Takes in a file/string/parsed PSL file, [parses it], grounds it, converts it to ADMM and calls Wolf, the S/C ADMM solver.
 * Returns the inferences of the unknown grounded predicates.
 */

case class InferenceResult(
  solution: ProblemSolution,
  idToGpMap: Map[Int, GroundedPredicate],
  objectiveFun: Option[Double] = None,
  groundingTime: Option[Long] = None,
  parsingTime: Option[Long] = None) {

  def getGp(predicate: String, individuals: String*): Option[GroundedPredicate] = {
    val individualList = individuals.toList
    val foundOption = idToGpMap.find {
      case (id, gp) =>
        gp.definition.name == predicate &&
          gp.groundings.map(_.name) == individualList
    }
    foundOption.map(_._2)
  }

  def truthValue(predicate: String, individuals: String*): Option[Double] = {
    val gpOption = getGp(predicate, individuals: _*)
    val truthValue = gpOption.map(gp => solution.results(gp.id))
    truthValue
  }

  override def toString() = {
    var s = solution.stats.toString
    s += printSelected()
    objectiveFun match {
      case Some(x) =>
        s += s"\nObjective function value: $x"
      case None =>
    }
    groundingTime match {
      case Some(x) =>
        s += s"\nGrounding time: $groundingTime"
      case None =>
    }
    s
  }

  def nicerTruthValue(t: Double): Double = {
    def goodEnough(v: Double): Boolean = math.abs(v - t) <= 1e-3
    lazy val fraction = 1.0 / (1.0 / t).round
    lazy val multipleTenths = (t * 10.0).round / 10.0
    lazy val multipleHundreds = (t * 100.0).round / 100.0
    if (goodEnough(0.0)) {
      0.0
    } else if (goodEnough(fraction)) {
      fraction
    } else if (goodEnough(multipleTenths)) {
      multipleTenths
    } else if (goodEnough(multipleHundreds)) {
      multipleHundreds
    } else {
      t
    }
  }

  def printSelected(predicateNames: List[String] = List.empty) = {
    var s = ""
    solution.results.foreach {
      case (id, truthValue) =>
        if (truthValue > 0) {
          val gp = idToGpMap(id)
          if (predicateNames.isEmpty || predicateNames.contains(gp.definition.name)) {
            s += s"\n$gp has truth value ${nicerTruthValue(truthValue)} [$truthValue]"
          }
        }
    }
    s
  }
}

case class InferencerConfig(
  asynchronous: Boolean = false,
  lazyThreshold: Option[Double] = Some(1e-13), // Absolute threshold for lazy vertices.
  breezeOptimizer: Boolean = true,
  globalConvergenceDetection: Option[Int] = Some(100), // Run convergence detection every 100 S/C steps.
  absoluteEpsilon: Double = 1e-8,
  relativeEpsilon: Double = 1e-3,
  computeObjectiveValueOfSolution: Boolean = true,
  objectiveLoggingEnabled: Boolean = false,
  maxIterations: Int = 10000, // maximum number of iterations.
  stepSize: Double = 1.0,
  tolerance: Double = 1e-12, // for Double precision is 15-17 decimal places, lower after arithmetic operations.
  isBounded: Boolean = true,
  serializeMessages: Boolean = false,
  removeSymmetricConstraints: Boolean = true,
  eagerSignalCollectConvergenceDetection: Boolean = true,
  heartbeatIntervalInMs: Int = 0) {

  def getWolfConfig = {
    WolfConfig(
      asynchronous = asynchronous,
      lazyThreshold = lazyThreshold,
      globalConvergenceDetection = globalConvergenceDetection,
      objectiveLoggingEnabled = objectiveLoggingEnabled,
      absoluteEpsilon = absoluteEpsilon,
      relativeEpsilon = relativeEpsilon,
      maxIterations = maxIterations,
      stepSize = stepSize,
      isBounded = isBounded,
      serializeMessages = serializeMessages,
      eagerSignalCollectConvergenceDetection = eagerSignalCollectConvergenceDetection,
      heartbeatIntervalInMs = heartbeatIntervalInMs)
  }
}

object Inferencer {

  /**
   *  Utility method that takes care also of the parsing of a file.
   */
  def runInferenceFromFile(
    pslFile: File,
    nodeActors: Option[Array[ActorRef]] = None,
    config: InferencerConfig = InferencerConfig()): InferenceResult = {
    val pslData = PslParser.parse(pslFile)
    runInference(pslData, nodeActors, config)
  }

  /**
   *  Utility method that takes care also of the parsing of a string.
   */
  def runInferenceFromString(
    pslFile: String,
    nodeActors: Option[Array[ActorRef]] = None,
    config: InferencerConfig = InferencerConfig()): InferenceResult = {
    val pslData = PslParser.parse(pslFile)
    runInference(pslData, nodeActors, config)
  }

  /**
   * Runs inference on the passed PSL file by using ADMM for consensus optimization.
   * Returns an InferenceResults, containing the ProblemSolution from Wolf, the mapping from predicates to ids
   * and optionally the value of the objective function.
   */
  def runInference(
    pslData: ParsedPslFile,
    nodeActors: Option[Array[ActorRef]] = None,
    config: InferencerConfig = InferencerConfig()): InferenceResult = {
    println(s"Running inferences for ${pslData.individuals.size} individuals ... ${if (pslData.individuals.size <= 10) pslData.individuals else "too many to list"}")
    // Ground the rules with the individuals.
    val (groundingResult, groundingTime) = Timer.time {
      Grounding.ground(pslData, config.isBounded, config.removeSymmetricConstraints)
    }
    val (groundedRules, groundedConstraints, idToGpMap) = groundingResult
    println(s"Grounding completed in $groundingTime ms: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")
    solveInferenceProblem(groundedRules, groundedConstraints, idToGpMap, groundingTime, nodeActors, config)
  }

  def solveInferenceProblem(groundedRules: Iterable[GroundedRule], groundedConstraints: Iterable[GroundedConstraint], idToGpMap: Map[Int, GroundedPredicate], groundingTime: Long, nodeActors: Option[Array[ActorRef]] = None, config: InferencerConfig = InferencerConfig()) = {
    val functions = groundedRules.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    val constraints = groundedConstraints.flatMap(_.createOptimizableFunction(config.stepSize, config.tolerance, config.breezeOptimizer))
    val functionsAndConstraints = functions ++ constraints
    println(s"Problem converted to consensus optimization with ${functions.size} functions and ${constraints.size} constraints that are not trivially true.")

    val solution = Wolf.solveProblem(
      functionsAndConstraints,
      nodeActors,
      config.getWolfConfig)
    //println(s"Problem solved, getting back results.")

    if (config.computeObjectiveValueOfSolution) {
      val objectiveFunctionVal = functionsAndConstraints.foldLeft(0.0) {
        case (sum, nextFunction) => sum + nextFunction.evaluateAt(solution.results)
      }
      //println("Computed the objective function.")
      InferenceResult(solution, idToGpMap, Some(objectiveFunctionVal), Some(groundingTime))
    } else {
      InferenceResult(solution, idToGpMap)
    }
  }
}
