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

trait Consensus {
  def consensusVotes: Array[Double]
  def consensus: Double
  def oldConsensus: Double
  def variableCount: Int
}

/**
 *  In the ADMM algorithm there are two types of nodes: consensus variable nodes and subproblem nodes.
 *  Each consensus variable node represents a variable on which value we want all the subproblem nodes to agree.
 *  For example, in PSL this variable is the truth value of a grounded predicate, like votes(anna, DP) = 0.6.
 *  Each consensus variable is connected with an edge to all subproblem nodes that use this variable.
 *  For example, in PSL the consensus variable "votes(anna, DP)" is connected to all the subproblems, i.e. grounded rules that contain
 *  "votes(anna, DP)", for example grounded rule "friend(bob, anna) AND votes(anna, DP) => votes (bob, DP)".
 */
class ConsensusVertex(
  variableId: Int, // the id of the variable, which identifies it also in the subproblem nodes.
  initialState: Double = 0.0, // the initial value for the consensus variable.
  isBounded: Boolean = true) // shall we use bounding (cutoff below 0 and above 1)? 
  extends MemoryEfficientDataGraphVertex[Double, Double](variableId, initialState) with Consensus {

  final def upperBound: Double = 1.0 // each consensus variable can only assume values in the range [lowerBound, upperBound].
  final def lowerBound: Double = 0.0

  type OutgoingSignalType = Double

  var hasCollected = false

  var signalsReceivedSinceCollect = 0

  def variableCount = _targetIds.size
  def consensus = state
  def oldConsensus = lastSignalState

  def collect = {
    hasCollected = true
    signalsReceivedSinceCollect = 0
    // New consensus is average vote.
    val newConsensus = averageConsensusVote
    if (isBounded) {
      bounded(newConsensus)
    } else {
      newConsensus
    }
  }

  /**
   * Overriding the internal S/C signal implementation.
   * We do not signal with the edges (so no need to define signal()),
   * instead the signalling is done here.
   */
  override def executeSignalOperation(graphEditor: GraphEditor[Int, Any]) {
    val signal = state
    _targetIds.foreach { targetId =>
      graphEditor.sendSignal(signal, targetId, id)
    }
    lastSignalState = state
  }

  /**
   * Overriding the internal S/C signal implementation.
   */
  override def deliverSignalWithSourceId(signal: Any, sourceId: Int, graphEditor: GraphEditor[Int, Any]): Boolean = {
    signalsReceivedSinceCollect += 1
    mostRecentSignalMap.put(sourceId, signal.asInstanceOf[Double])
    false
  }

  /**
   * We send the same signal along all edges, which
   * is efficiently done in 'executeSignalOperation'.
   * This function should therefore never be called.
   */
  def computeSignal(targetId: Int): Double = {
    throw new UnsupportedOperationException
  }

  def consensusVotes: Array[Double] = {
    val votes = new Array[Double](_targetIds.size)
    var i = 0
    _targetIds.foreach { targetId =>
      votes(i) = mostRecentSignalMap(targetId)
      i += 1
    }
    votes
  }

  def averageConsensusVote: Double = {
    consensusVoteSum / _targetIds.size
  }

  def consensusVoteSum: Double = {
    // We don't trust the performance of doing it functionally. :P
    var sum = 0.0
    var i = 0
    val votes = mostRecentSignalMap.values
    val length = votes.length
    while (i < length) {
      sum += votes(i)
      i += 1
    }
    sum
  }

  override def scoreCollect = {
    if (signalsReceivedSinceCollect == _targetIds.size) {
      1
    } else {
      0
    }
  }

  /**
   * Compute whether we should send signals to the subproblem vertices.
   *  Note: The computation starts in the subproblem vertices.
   *  This is empirically faster.
   *  On the other side, this doesn't allow to initialize the consensus vars
   *  to anything except zero.
   *  This is perfectly fine with PSL inference, but may have drawbacks in other cases.
   *
   *  Only try to signal if the vertex has collected before.
   */
  override def scoreSignal = {
    if (hasCollected) {
      1
    } else {
      0
    }
  }

  def bounded(i: Double): Double = {
    math.max(math.min(i, upperBound), lowerBound)
  }

}
