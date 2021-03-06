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
  isBounded: Boolean, // shall we use bounding (cutoff below 0 and above 1)? 
  lowerBound: Double = 0.0, // each consensus variable can only assume values in the range [lowerBound, upperBound].
  upperBound: Double = 1.0)
  extends ConsensusVertex(variableId, initialState, isBounded, lowerBound, upperBound) {

  /**
   * Always signal after the first collect:
   * We only get new votes if something has actually changed, so it always makes sense to tell everyone about the change.
   * This has the advantage that a subproblem vertex that sends a signal to a consensus vertex can rely on being scheduled again.
   */
  override def scoreSignal = {
    if (hasCollectedOnce) {
      1.0
    } else {
      0.0
    }
  }

}
