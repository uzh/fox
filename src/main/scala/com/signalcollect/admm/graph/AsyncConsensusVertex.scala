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

/**
 *  Adaptations to the consensus vertex that optimize it for asynchronous execution.
 */
final class AsyncConsensusVertex(
  variableId: Int, // the id of the variable, which identifies it also in the subproblem nodes.
  initialState: Double = 0.0, // the initial value for the consensus variable.
  isBounded: Boolean = true) // shall we use bounding (cutoff below 0 and above 1)? 
  extends ConsensusVertex(variableId, initialState, isBounded) {

  var shouldSignal = false
  var signalsReceivedSinceCollect = 0

  override def executeSignalOperation(graphEditor: GraphEditor[Int, Any]) {
    shouldSignal = false
    super.executeSignalOperation(graphEditor)
  }

  override def deliverSignalWithSourceId(signal: Any, sourceId: Int, graphEditor: GraphEditor[Int, Any]): Boolean = {
    signalsReceivedSinceCollect += 1
    mostRecentSignalMap.put(sourceId, signal.asInstanceOf[Double])
    if (signalsReceivedSinceCollect == _targetIds.size) {
      shouldSignal = true
      signalsReceivedSinceCollect = 0
      state = collect
    }
    true
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
