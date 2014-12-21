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
  val SKIP_COLLECT = -1
  val ENSURE_COLLECT = -2
}

/**
 * Lazy version of the subproblem vertex, only signals to the consensus vertex if the signal changes.
 * If no signal changes, but the multipliers still changed, the vertex ensures that it will get
 * scheduled again by sending special messages to itself.
 *
 * The skip collect message ensures this vertex skips the next step, which is the one during which
 * the consensus vertices collect. The ensure collect message gets sent in the subsequent step and
 * ensures that S/C will schedule the vertex, even if no consensus vertex sent a signal to it.
 */
final class LazySubproblemVertex(
  subproblemId: Int, // The id of the subproblem.
  optimizableFunction: OptimizableFunction, // The function that is contained in the subproblem.
  val absoluteSignallingThreshold: Double)
  extends SubproblemVertex(subproblemId, optimizableFunction) {

  val multipliersLength = multipliers.length

  /**
   * Ensure last signal state is all 0, which corresponds to having implicitly sent a 0.
   */
  lastSignalState = new Array[Double](multipliersLength)

  var lastMultipliers = multipliers.clone

  @inline def changed(a: Double, b: Double): Boolean = {
    val delta = math.abs(a - b)
    delta > absoluteSignallingThreshold
  }

  @inline def atLeastOneMultiplierChanged: Boolean = {
    val currentMultipliers = multipliers
    var i = 0
    while (i < multipliersLength) {
      if (changed(lastMultipliers(i), currentMultipliers(i))) {
        return true
      }
      i += 1
    }
    false
  }

  /**
   * Only send a signal if the signal is different from what was sent last time around.
   * Implicitly last time a 0 was sent.
   */
  override def executeSignalOperation(graphEditor: GraphEditor[Int, Double]) {
    var alreadySentId = Set.empty[Int]
    val idToIndexMapping = optimizableFunction.idToIndexMappings
    var i = 0
    var atLeastOneSignalSent = false
    while (i < multipliersLength) {
      val targetId = idToIndexMapping(i)
      if (!alreadySentId.contains(targetId)) {
        val targetIdValue = state(i)
        val signalChanged = changed(lastSignalState(i), targetIdValue)
        if (signalChanged) {
          atLeastOneSignalSent = true
          graphEditor.sendSignal(targetIdValue, targetId, id)
          lastSignalState(i) = targetIdValue
          lastMultipliers(i) = multipliers(i)
        }
        alreadySentId += targetId
      }
      i += 1
    }
    // If we signaled to a consensus vertex, then we're guaranteed to get woken up again.
    // If we did not signal, but the multipliers changed, then we want to schedule ourselves.
    if (!atLeastOneSignalSent && atLeastOneMultiplierChanged) {
      graphEditor.sendSignal(MSG.SKIP_COLLECT, id, id)
      lastMultipliers = multipliers.clone
    }
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
