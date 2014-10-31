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

object MSG {
  val SKIP_COLLECT = Double.NaN
  val ENSURE_COLLECT = Double.NegativeInfinity
}

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

  var lastMultipliers = multipliers.clone

  def absoluteThreshold = 1e-16

  def changed(a: Double, b: Double): Boolean = {
    val delta = math.abs(a - b)
    delta > absoluteThreshold
  }

  /**
   * Only send a signal if the signal is different from what was sent las time around.
   * Implicitly last time a 0 was sent.
   */
  override def executeSignalOperation(graphEditor: GraphEditor[Int, Double]) {
    var alreadySentId = Set.empty[Int]
    val idToIndexMapping = optimizableFunction.idToIndexMappings
    val idToIndexMappingLength = idToIndexMapping.length
    var i = 0
    val currentMultipliers = multipliers
    var atLeastOneSignalSent = false
    var atLeastOneMultiplierChanged = false
    while (i < idToIndexMappingLength) {
      val targetId = idToIndexMapping(i)
      if (!alreadySentId.contains(targetId)) {
        val targetIdValue = state(i)
        val signalChanged = changed(lastSignalState(i), targetIdValue)
        atLeastOneMultiplierChanged = atLeastOneMultiplierChanged || changed(lastMultipliers(i), currentMultipliers(i))
        if (signalChanged) {
          atLeastOneSignalSent = true
          graphEditor.sendSignal(targetIdValue, targetId, id)
        }
        alreadySentId += targetId
      }
      i += 1
    }
    if (!atLeastOneSignalSent && atLeastOneMultiplierChanged) {
      // We do not change our signal right now, but we would like to get scheduled again, because the mutipliers changed.
      graphEditor.sendSignal(MSG.SKIP_COLLECT, id, id)
    }
    lastSignalState = state.clone
    lastMultipliers = multipliers.clone
  }

  var skipCollect = false

  override def deliverSignalWithSourceId(signal: Double, sourceId: Int, graphEditor: GraphEditor[Int, Double]): Boolean = {
    if (sourceId == id) {
      if (signal == MSG.SKIP_COLLECT) {
        skipCollect = true
        graphEditor.sendSignal(MSG.ENSURE_COLLECT, id, id)
      } else if (signal == MSG.ENSURE_COLLECT) {
        skipCollect = false
      }
      false
    } else {
      super.deliverSignalWithSourceId(signal, sourceId, graphEditor)
    }
  }

  override def scoreCollect = if (skipCollect) 0 else 1

}
