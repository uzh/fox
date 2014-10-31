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

package com.signalcollect.admm.graph

import com.signalcollect.GraphEditor
import com.signalcollect.MemoryEfficientDataGraphVertex
import com.signalcollect.admm.optimizers.OptimizableFunction

trait Subproblem {
  def multipliers: Array[Double]
  def optimizableFunction: OptimizableFunction
  def consensusAssignments: Array[Double]
}

/**
 *  In the ADMM algorithm there are two types of nodes: consensus variable nodes and subproblem nodes.
 *  Each subproblem node represents a piece of the big problem that can be solved independently, which is an optimizable function.
 *  For example, in PSL this node represents the truth value of a grounded rule, e.g. "friend(bob, anna) AND votes(anna, DP) => votes (bob, DP)".
 *  Each subproblem node is connected to all the consensus variables of the variables it uses.
 *  For example, in PSL the subproblem "friend(bob, anna) AND votes(anna, DP) => votes (bob, DP)" is connected to
 *  the consensus variables "votes(anna, DP)", "votes(bob, DP)" and "friend(bob, anna)".
 */
class SubproblemVertex(
  subproblemId: Int, // The id of the subproblem.
  val optimizableFunction: OptimizableFunction,
  implicitZero: Boolean) // The function that is contained in the subproblem.
  extends MemoryEfficientDataGraphVertex[Array[Double], Double, Double](subproblemId, null.asInstanceOf[Array[Double]])
  with Subproblem {

  type OutgoingSignalType = Double

  def multipliers = optimizableFunction.getYEfficient

  /**
   * Overriding the internal S/C signal implementation.
   * We do not signal with the edges (so no need to define signal()),
   * instead the signalling is done here.
   */
  override def executeSignalOperation(graphEditor: GraphEditor[Int, Double]) {
    // TODO: We don't really need the S/C vertex target ids. Can we save some memory here?
    var alreadySentId = Set.empty[Int]
    val idToIndexMapping = optimizableFunction.idToIndexMappings
    val idToIndexMappingLength = idToIndexMapping.length
    var i = 0
    while (i < idToIndexMappingLength) {
      val targetId = idToIndexMapping(i)
      if (!alreadySentId.contains(targetId)) {
        val targetIdValue = state(i)
        if (targetIdValue != 0 || !implicitZero) {
          graphEditor.sendSignal(targetIdValue, targetId, id)
        }
        alreadySentId += targetId
      }
      i += 1
    }
  }

  override def afterInitialization(graphEditor: GraphEditor[Int, Double]) {
    executeCollectOperation(graphEditor)
  }

  // Collect all the connected consensus variable values.
  def consensusAssignments: Array[Double] = {
    val idToIndexMapping = optimizableFunction.idToIndexMappings
    val idToIndexMappingLength = idToIndexMapping.length
    val consensusVariableAssigments = new Array[Double](idToIndexMappingLength)
    var i = 0
    while (i < idToIndexMappingLength) {
      val idAtIndex = idToIndexMapping(i)
      consensusVariableAssigments(i) = mostRecentSignalMap(idAtIndex)
      i += 1
    }
    consensusVariableAssigments
  }

  def collect: Array[Double] = {
    val consensus = consensusAssignments
    // Update the lagrangian multipliers (y) : y-step
    optimizableFunction.updateLagrangeEfficient(consensus)
    // Minimize the local function and get argmin (x) : x-step
    optimizableFunction.optimizeEfficient(consensus)
    val newOptimizedAssignments = optimizableFunction.getX
    newOptimizedAssignments
  }

  override def scoreCollect = 1
  // Always signal, even in the first iteration, when the consensus variable doesn't.
  override def scoreSignal = 1

  /**
   * Signalling is efficiently done in 'executeSignalOperation'.
   * This function should therefore never be called.
   */
  override def computeSignal(targetId: Int): Double = {
    throw new UnsupportedOperationException
  }
}
