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

package com.signalcollect.admm

import com.signalcollect.Graph
import com.signalcollect.admm._
import com.signalcollect.admm.graph.ConsensusVertex
import com.signalcollect.admm.graph.SubproblemVertex
import com.signalcollect.admm.graph.DummyEdge
import com.signalcollect.admm.graph.DummyEdge
import com.signalcollect.admm.graph.SubproblemVertex
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.ExecutionConfiguration
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.GraphBuilder
import com.signalcollect.util.IntDoubleHashMap
import com.signalcollect.util.IntDoubleHashMap

object AdmmTestHelper {

  def createZMap(z: Array[Double]) = {
    (0 until z.length).map(createId).zip(z).toMap
  }

  def createEmptyMap(z: Array[Double]) = {
    (0 until z.length).map(v => (createId(v), 0.0)).toMap
  }

  def createId(i: Int) = {
    i
  }

  def decodeId(s: String) = {
    Integer.parseInt(s)
  }

  def createSubproblemId(i: Int) = {
    -i
  }

  def consensus(graph: Graph[Any, Any], id: Int): ConsensusVertex = {
    graph.forVertexWithId(id, {
      v: ConsensusVertex => v
    })
  }

  def subproblem(graph: Graph[Any, Any], id: Int): SubproblemVertex = {
    graph.forVertexWithId(id, {
      v: SubproblemVertex => v
    })
  }

  def createGraph(z: Array[Double], hlts: Map[Int, List[OptimizableFunction]], stepSize: Double): IntDoubleHashMap = {
    val emptyMap = AdmmTestHelper.createEmptyMap(z)
    val graph = new GraphBuilder[Int, Double]().withConsole(false).build

    var id = 0

    for { i <- 0 until z.length } {
      graph.addVertex(new ConsensusVertex(createId(i), z(i)))

      for { j <- 0 until hlts(i).length } {
        graph.addVertex(new SubproblemVertex(createSubproblemId(id), hlts(i)(j), Array()))
        // Edges in both directions.
        graph.addEdge(createSubproblemId(id), new DummyEdge(createId(i)))
        graph.addEdge(createId(i), new DummyEdge(createSubproblemId(id)))
        id += 1
      }
    }

    val globalConvergence = new GlobalAdmmConvergenceDetection()
    val stats = graph.execute(ExecutionConfiguration().
      withExecutionMode(ExecutionMode.Synchronous).
      withGlobalTerminationDetection(globalConvergence))
    val results = graph.aggregate(ConsensusAggregator)
    graph.shutdown
    results.getOrElse(new IntDoubleHashMap(1, 0.5f))
  }

  def createGraph(z: Array[Double], hlt: OptimizableFunction, stepSize: Double): IntDoubleHashMap = {
    createGraph(z, (0 until z.length).map(v => (v -> List(hlt))).toMap, stepSize)
  }

}