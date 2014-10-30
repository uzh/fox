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
 * Lazy version of the subproblem vertex, only signals if something has changed.
 */
class LazySubproblemVertex(
  subproblemId: Int, // The id of the subproblem.
  optimizableFunction: OptimizableFunction) // The function that is contained in the subproblem.
  extends SubproblemVertex(subproblemId, optimizableFunction) {

  /**
   * Ensure last signal state is all 0, which corresponds to having implicitly sent a 0.
   */
  lastSignalState = new Array[Double](multipliers.length)

  /**
   * Only send a signal if the signal is different from what was sent last time around.
   * Implicitly last time a 0 was sent.
   */
  override def executeSignalOperation(graphEditor: GraphEditor[Int, Double]) {
    var alreadySentId = Set.empty[Int]
    val idToIndexMapping = optimizableFunction.idToIndexMappings
    val idToIndexMappingLength = idToIndexMapping.length
    var i = 0
    while (i < idToIndexMappingLength) {
      val targetId = idToIndexMapping(i)
      if (!alreadySentId.contains(targetId)) {
        val targetIdValue = state(i)
        if (targetIdValue != lastSignalState(i)) {
          graphEditor.sendSignal(targetIdValue, targetId, id)
        }
        alreadySentId += targetId
      }
      i += 1
    }
    lastSignalState = state.clone
  }

}
