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

package com.signalcollect.admm

import scala.collection.immutable.TreeMap

import com.signalcollect.GlobalTerminationDetection
import com.signalcollect.Graph
import com.signalcollect.MultiAggregator
import com.signalcollect.Vertex
import com.signalcollect.admm.graph.Consensus
import com.signalcollect.admm.graph.Subproblem
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.interfaces.ModularAggregationOperation

case class GlobalAdmmConvergenceDetection(
  stepSize: Double = 1.0,
  absoluteEpsilon: Double = 10e-8,
  relativeEpsilon: Double = 10e-3,
  override val checkingInterval: Long = 2)
  extends AbstractGlobalAdmmConvergenceDetection {
  type ResultType = (PrimalData, DualData)
  val aggregationOperation = MultiAggregator(PrimalAggregator, DualAggregator)
  def shouldTerminate(t: (PrimalData, DualData)) = isConverged(t)
  def shouldTerminate(g: Graph[Int, Any]): Boolean = {
    val aggregationResult = g.aggregate(aggregationOperation)
    shouldTerminate(aggregationResult)
  }
}

case class GlobalAdmmConvergenceDetectionWithDebugging(
  functions: Traversable[OptimizableFunction],
  stepSize: Double = 1.0,
  absoluteEpsilon: Double = 10e-8,
  relativeEpsilon: Double = 10e-3,
  override val checkingInterval: Long = 2)
  extends AbstractGlobalAdmmConvergenceDetection {
  var objectiveValueForSteps: TreeMap[Int, Double] = TreeMap()
  type ResultType = ((PrimalData, DualData), Option[Map[Int, Double]])
  val aggregationOperation = MultiAggregator(MultiAggregator(PrimalAggregator, DualAggregator), ObjectiveValueAggregator)
  def shouldTerminate(t: ((PrimalData, DualData), Double)) = {
    if (collectStepsSoFar % checkingInterval == 0) {
      val (convergenceData, objectiveValue) = t
      val converged = isConverged(convergenceData, true)
      println(s"Objective value @ convergence detection step $convergenceDetectionStep = $objectiveValue")
      objectiveValueForSteps += convergenceDetectionStep -> objectiveValue
      converged
    } else {
      collectStepsSoFar += 1
      false
    }
  }
  def shouldTerminate(g: Graph[Int, Any]): Boolean = {
    val aggregationResult = g.aggregate(aggregationOperation)
    shouldTerminate(aggregationResult)
  }
}

abstract class AbstractGlobalAdmmConvergenceDetection extends GlobalTerminationDetection[Int, Any] {
  override def aggregationInterval = 1
  def stepSize: Double
  def absoluteEpsilon: Double
  def relativeEpsilon: Double
  def checkingInterval: Long
  assert(checkingInterval % 2 == 0, "Checking interval has to be an even number.")
  var primalResidualForSteps: TreeMap[Int, Double] = TreeMap()
  var primalEpsilonForSteps: TreeMap[Int, Double] = TreeMap()
  var dualResidualForSteps: TreeMap[Int, Double] = TreeMap()
  var dualEpsilonForSteps: TreeMap[Int, Double] = TreeMap()
  var collectStepsSoFar = 0 // Execution always signals, then collects, then checks for global convergence.

  def convergenceDetectionStep = (collectStepsSoFar / checkingInterval).toInt + 1

  var nextConvergenceOutputAtPercentage = 0

  def isConverged(t: (PrimalData, DualData), debugLogging: Boolean = false): Boolean = {
    collectStepsSoFar += 1
    if (collectStepsSoFar % checkingInterval == 1) {
      val (primalData, dualData) = t
      val primalConvergence = computePrimalConvergence(primalData, debugLogging)
      val dualConvergence = computeDualConvergence(primalData, dualData, debugLogging)
      val convergencePercentage = math.min(math.min(primalConvergence, dualConvergence), 1.0) * 100
      if (nextConvergenceOutputAtPercentage == 0) {
        print("[")
      } else {
        print("-")
      }
      while (convergencePercentage >= nextConvergenceOutputAtPercentage) {
        print(s"$nextConvergenceOutputAtPercentage%")
        if (nextConvergenceOutputAtPercentage != 100) {
          print("-")
        } else {
          print(s"]")
          println
        }
        nextConvergenceOutputAtPercentage += 10
      }
      if (debugLogging) { println(s"Primal convergence = $primalConvergence, dual convergence = $dualConvergence") }
      val shouldTerminate = primalConvergence >= 1.0 && dualConvergence >= 1.0
      shouldTerminate
    } else {
      false
    }
  }

  /**
   * Returns dualEpsilon / dualResidualForPreviousStep if dualResidualForPreviousStep is defined,
   * else 0.0
   */
  def computeDualConvergence(p: PrimalData, d: DualData, debugLogging: Boolean): Double = {
    val dualEpsilon = {
      absoluteEpsilon * math.sqrt(p.numberOfLocalVars) +
        relativeEpsilon * math.sqrt(d.sumOfSquaredMultipliers)
    }
    val dualResidual = stepSize * math.sqrt(d.sumOfSquaredConsensusDeltas)
    dualEpsilonForSteps += convergenceDetectionStep -> dualEpsilon
    dualResidualForSteps += convergenceDetectionStep -> dualResidual
    val dualConvergence = {
      if (dualResidual == 0) {
        Double.MaxValue
      } else {
        dualEpsilon / dualResidual
      }
    }
    if (debugLogging) {
      println(s"Dual epsilon @ convergence detection step $convergenceDetectionStep = $dualEpsilon")
      println(s"Dual residual @ convergence detection step $convergenceDetectionStep = $dualResidual")
      //      println(s"NumberOfLocalVars @ convergence detection step $step = ${p.numberOfLocalVars}")    
      //      println(s"SumOfSquaredMultipliers @ convergence detection step $step = ${d.sumOfSquaredMultipliers}")
      println(s"SumOfSquaredConsensusDeltas @ convergence detection step $convergenceDetectionStep = ${d.sumOfSquaredConsensusDeltas}")
    }
    dualConvergence
  }

  /**
   * Returns primalEpsilonForPreviousStep / primalResidualForPreviousStep if primalResidualForPreviousStep is defined,
   * else 0.0
   */
  def computePrimalConvergence(p: PrimalData, debugLogging: Boolean): Double = {
    val primalEpsilon = {
      absoluteEpsilon * math.sqrt(p.numberOfLocalVars) +
        relativeEpsilon * math.max(math.sqrt(p.sumOfSquaredLocalVars), math.sqrt(p.sumOfSquaredConsensus))
    }
    val primalResidual = math.sqrt(p.sumOfSquaredErrors)
    if (debugLogging) {
      println(s"Primal epsilon @ convergence detection step $convergenceDetectionStep = $primalEpsilon")
      println(s"Primal residual @ convergence detection step $convergenceDetectionStep = $primalResidual")
      //      println(s"NumberOfLocalVars @ convergence detection step $step = ${p.numberOfLocalVars}")    
      //      println(s"SumOfSquaredLocalVars @ convergence detection step $step = ${p.sumOfSquaredLocalVars}")
      //      println(s"SumOfSquaredConsensus @ convergence detection step $step = ${p.sumOfSquaredConsensus}")
    }
    primalEpsilonForSteps += convergenceDetectionStep -> primalEpsilon
    primalResidualForSteps += convergenceDetectionStep -> primalResidual
    val primalResidualForPreviousStep = primalResidualForSteps.get(convergenceDetectionStep - 1)
    val primalEpsilonForPreviousStep = primalEpsilonForSteps.get(convergenceDetectionStep - 1)
    val primalConvergence = {
      if (primalResidualForPreviousStep.isDefined && primalEpsilonForPreviousStep.isDefined) {
        if (primalResidualForPreviousStep.get == 0) {
          Double.MaxValue
        } else {
          primalEpsilonForPreviousStep.get / primalResidualForPreviousStep.get
        }
      } else {
        0.0
      }
    }
    primalConvergence
  }
}

case class PrimalData(
  sumOfSquaredErrors: Double,
  sumOfSquaredConsensus: Double,
  sumOfSquaredLocalVars: Double,
  numberOfLocalVars: Int) {
  def +(other: PrimalData): PrimalData = {
    PrimalData(
      sumOfSquaredErrors + other.sumOfSquaredErrors,
      sumOfSquaredConsensus + other.sumOfSquaredConsensus,
      sumOfSquaredLocalVars + other.sumOfSquaredLocalVars,
      numberOfLocalVars + other.numberOfLocalVars)
  }
}

case class DualData(
  sumOfSquaredConsensusDeltas: Double,
  sumOfSquaredMultipliers: Double) {
  def +(other: DualData): DualData = {
    DualData(
      sumOfSquaredConsensusDeltas + other.sumOfSquaredConsensusDeltas,
      sumOfSquaredMultipliers + other.sumOfSquaredMultipliers)
  }
}

case object PrimalAggregator extends ModularAggregationOperation[PrimalData] {
  val neutralElement = PrimalData(0, 0, 0, 0)
  def extract(v: Vertex[_, _, _, _]): PrimalData = {
    v match {
      case c: Consensus =>
        val votes = c.consensusVotes
        val consensus = c.consensus
        val numberOfLocalVars = c.variableCount
        var sumOfSquaredErrors = 0.0
        var sumOfSquaredLocalVars = 0.0
        var i = 0
        while (i < votes.length) {
          val vote = votes(i)
          val error = consensus - vote
          val squaredError = error * error
          sumOfSquaredErrors += squaredError
          sumOfSquaredLocalVars += vote * vote
          i += 1
        }
        val sumOfSquaredConsensus = {
          consensus * consensus * numberOfLocalVars
        }
        PrimalData(sumOfSquaredErrors, sumOfSquaredConsensus, sumOfSquaredLocalVars, numberOfLocalVars)
      case other =>
        neutralElement
    }
  }
  def aggregate(a: PrimalData, b: PrimalData): PrimalData = a + b
}

case object DualAggregator extends ModularAggregationOperation[DualData] {
  val neutralElement = DualData(0, 0)
  def extract(v: Vertex[_, _, _, _]): DualData = {
    v match {
      case c: Consensus =>
        val consensus = c.consensus
        val oldConsensus = c.oldConsensus
        val consensusDelta = consensus - oldConsensus
        val sumOfSquaredConsensusDeltas = consensusDelta * consensusDelta
        DualData(sumOfSquaredConsensusDeltas, 0)
      case s: Subproblem =>
        var sumOfSquaredMultipliers = 0.0
        val multipliers = s.multipliers
        var i = 0
        while (i < multipliers.length) {
          val multiplier = multipliers(i)
          sumOfSquaredMultipliers += multiplier * multiplier
          i += 1
        }
        DualData(0, sumOfSquaredMultipliers)
      case other =>
        neutralElement
    }
  }
  def aggregate(a: DualData, b: DualData): DualData = a + b
}