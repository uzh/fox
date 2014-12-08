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

/**
 *  In the ADMM algorithm there are two types of nodes: consensus variable nodes and subproblem nodes.
 *  Each subproblem node represents a piece of the big problem that can be solved independently, which is an optimizable function.
 *  For example, in PSL this node represents the truth value of a grounded rule, e.g. "friend(bob, anna) AND votes(anna, DP) => votes (bob, DP)".
 *  Each subproblem node is connected to all the consensus variables of the variables it uses.
 *  For example, in PSL the subproblem "friend(bob, anna) AND votes(anna, DP) => votes (bob, DP)" is connected to
 *  the consensus variables "votes(anna, DP)", "votes(bob, DP)" and "friend(bob, anna)".
 */
class AsyncSubproblemVertex(
  subproblemId: Int, // The id of the subproblem.
  optimizableFunction: OptimizableFunction) // The function that is contained in the subproblem.
  extends SubproblemVertex(subproblemId, optimizableFunction) {

  var signalsReceivedSinceCollect = 0

  override def executeSignalOperation(graphEditor: GraphEditor[Int, Double]) {
    super.executeSignalOperation(graphEditor)
    shouldSignal = false
  }

  var shouldSignal = true

  /**
   * Overriding the internal S/C signal implementation.
   */
  override def deliverSignalWithSourceId(signal: Double, sourceId: Int, graphEditor: GraphEditor[Int, Double]): Boolean = {
    signalsReceivedSinceCollect += 1
    mostRecentSignalMap.put(sourceId, signal)
    if (signalsReceivedSinceCollect == _targetIds.size) {
      signalsReceivedSinceCollect = 0
      state = collect
      shouldSignal = true
      true
    } else {
      false
    }
  }

  override def scoreCollect = 0

  override def scoreSignal = {
    if (shouldSignal) {
      1
    } else {
      0
    }
  }

}
