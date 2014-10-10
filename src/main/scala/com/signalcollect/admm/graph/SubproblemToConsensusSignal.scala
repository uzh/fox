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

/**
 * Signal sent from a subproblem vertex to a consensus vertex.
 * Besides the consensusVote, which contains the value of the local variable x
 * related to the global variable z represented by the consensus vertex,
 * we send also the value of the multiplier (y).
 */
case class SubproblemToConsensusSignal(
  consensusVote: Double,
  multiplierY: Double,
  isLocallyConverged: Boolean)