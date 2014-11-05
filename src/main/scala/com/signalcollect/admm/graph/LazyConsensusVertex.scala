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

/**
 * Lazy version of the consensus vertex, only signals if something has changed.
 */
final class LazyConsensusVertex(
  variableId: Int, // the id of the variable, which identifies it also in the subproblem nodes.
  initialState: Double, // the initial value for the consensus variable.
  isBounded: Boolean,
  val absoluteSignallingThreshold: Double) // shall we use bounding (cutoff below 0 and above 1)? 
  extends ConsensusVertex(variableId, initialState, isBounded) {

  @inline def changed(a: Double, b: Double): Boolean = {
    val delta = math.abs(a - b)
    delta > absoluteSignallingThreshold
  }

  /**
   * We signal if things have changed or the consensus value has not under or overflown.
   */
  override def scoreSignal = {
    if (hasCollectedOnce &&
      (changed(consensus, lastSignalState) || (consensus != upperBound && consensus != lowerBound))) {
      1.0
    } else {
      0.0
    }
  }

}
