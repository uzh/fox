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

import scala.reflect.runtime.universe

import com.signalcollect.Edge
import com.signalcollect.ExecutionConfiguration
import com.signalcollect.ExecutionInformation
import com.signalcollect.Graph
import com.signalcollect.GraphBuilder
import com.signalcollect.Vertex
import com.signalcollect.admm.graph.AsyncConsensusVertex
import com.signalcollect.admm.graph.AsyncSubproblemVertex
import com.signalcollect.admm.graph.Consensus
import com.signalcollect.admm.graph.ConsensusVertex
import com.signalcollect.admm.graph.DummyEdge
import com.signalcollect.admm.graph.PslSchedulerFactory
import com.signalcollect.admm.graph.Subproblem
import com.signalcollect.admm.graph.SubproblemVertex
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.admm.utils.Timer
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.configuration.TerminationReason
import com.signalcollect.factory.messagebus.BulkAkkaMessageBusFactory
import com.signalcollect.factory.scheduler.Throughput
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandler
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandlerFactory
import com.signalcollect.interfaces.ModularAggregationOperation
import com.signalcollect.util.IntDoubleHashMap

import akka.actor.ActorRef

case class ProblemSolution(
  stats: ExecutionInformation[Int, Double],
  results: IntDoubleHashMap,
  convergence: Option[AbstractGlobalAdmmConvergenceDetection] = None,
  graphLoadingTime: Long) // in case we have local convergence this is None.

case class WolfConfig(
  asynchronous: Boolean = false,
  globalConvergenceDetection: Option[Int] = Some(2), // Detect global convergence every 2 S/C steps by default. Note: should be a multiple of 2 for making sure convergence works.
  absoluteEpsilon: Double = 1e-8,
  relativeEpsilon: Double = 1e-3,
  objectiveLoggingEnabled: Boolean = false,
  maxIterations: Int = 2000, // maximum number of iterations.
  stepSize: Double = 1.0,
  isBounded: Boolean = true,
  serializeMessages: Boolean = false,
  eagerSignalCollectConvergenceDetection: Boolean = true,
  heartbeatIntervalInMs: Int = 0)

case class NonExistentConsensusVertexHandlerFactory(
  asynchronous: Boolean, // If the execution is asynchronous
  initialState: Double, // the initial value for the consensus variable.
  isBounded: Boolean // shall we use bounding (cutoff below 0 and above 1)? 
  ) extends EdgeAddedToNonExistentVertexHandlerFactory[Int, Double] {
  def createInstance: EdgeAddedToNonExistentVertexHandler[Int, Double] =
    new NonExistentConsensusVertexHandler(asynchronous, initialState, isBounded)
  override def toString = "NoneExistentConsensusVertexFactory"
}

case class NonExistentConsensusVertexHandler(
  asynchronous: Boolean, // If the execution is asynchronous
  initialState: Double, // the initial value for the consensus variable.
  isBounded: Boolean // shall we use bounding (cutoff below 0 and above 1)? 
  ) extends EdgeAddedToNonExistentVertexHandler[Int, Double] {
  def handleImpossibleEdgeAddition(edge: Edge[Int], vertexId: Int): Option[Vertex[Int, _, Int, Double]] = {
    if (asynchronous) {
      Some(
        new AsyncConsensusVertex(
          variableId = vertexId,
          initialState = initialState,
          isBounded = isBounded))
    } else {
      Some(
        new ConsensusVertex(
          variableId = vertexId,
          initialState = initialState,
          isBounded = isBounded))
    }
  }
}

// Solves problems.
object Wolf {
  def solveProblem(
    functions: Traversable[OptimizableFunction],
    nodeActors: Option[Array[ActorRef]] = None,
    config: WolfConfig = new WolfConfig()): ProblemSolution = {
    val (graph, graphLoadingTime) = Timer.time {
      createGraph(functions, nodeActors, config, config.serializeMessages)
    }
    println(s"ADMM graph creation completed in $graphLoadingTime ms.")
    try {
      println("Starting inference ...")
      val (stats, convergence) = if (config.globalConvergenceDetection.isDefined) {
        // Global convergence case:
        val globalConvergence = if (config.objectiveLoggingEnabled) {
          GlobalAdmmConvergenceDetectionWithDebugging(
            functions,
            absoluteEpsilon = config.absoluteEpsilon,
            relativeEpsilon = config.relativeEpsilon,
            checkingInterval = config.globalConvergenceDetection.get)
        } else {
          GlobalAdmmConvergenceDetection(
            absoluteEpsilon = config.absoluteEpsilon,
            relativeEpsilon = config.relativeEpsilon,
            checkingInterval = config.globalConvergenceDetection.get)
        }
        //println("Global convergence detection initialized.")
        val stats = graph.execute(ExecutionConfiguration[Int, Double]().
          withExecutionMode(if (config.asynchronous) ExecutionMode.PureAsynchronous else ExecutionMode.Synchronous).
          withGlobalTerminationDetection(globalConvergence).
          withStepsLimit(config.maxIterations))
        val convergenceMessage = stats.executionStatistics.terminationReason match {
          case TerminationReason.TimeLimitReached =>
            "Computation finished because the time limit was reached."
          case TerminationReason.Converged =>
            "Computation finished because setting all the variables to 0 is a solution."
          case TerminationReason.GlobalConstraintMet =>
            "Computation finished because the global error was small enough."
          case TerminationReason.ComputationStepLimitReached =>
            "Computation finished because the steps limit was reached."
          case TerminationReason.TerminatedByUser =>
            "Computation terminated on user request."
        }
        println(convergenceMessage)
        (stats, Some(globalConvergence))
      } else {
        val stats = graph.execute(ExecutionConfiguration().
          withExecutionMode(ExecutionMode.Synchronous).
          withStepsLimit(config.maxIterations))
        println("Reached max iterations, retrieving the results.")
        (stats, None)
      }
      val resultMap = graph.aggregate(ConsensusAggregator)
      val solution = ProblemSolution(
        stats = stats,
        results = resultMap.getOrElse(new IntDoubleHashMap(initialSize = 1, rehashFraction = 0.5f)),
        convergence = convergence,
        graphLoadingTime = graphLoadingTime)
      solution
    } finally {
      graph.shutdown
    }
  }

  def createGraph(
    functions: Traversable[OptimizableFunction],
    nodeActors: Option[Array[ActorRef]] = None,
    config: WolfConfig = new WolfConfig(),
    serializeMessages: Boolean = false): Graph[Int, Double] = {
    //println(s"Creating the ADMM graph ...")
    // Use node actors with graph builder, if they have been passed.
    val consensusHandlerFactory = new NonExistentConsensusVertexHandlerFactory(
      asynchronous = config.asynchronous,
      initialState = 0.0, // the initial value for the consensus variable.
      isBounded = config.isBounded // shall we use bounding (cutoff below 0 and above 1)? 
      )
    val graphBuilder = {
      nodeActors.map(new GraphBuilder[Int, Double]().withPreallocatedNodes(_)).
        getOrElse(new GraphBuilder[Int, Double]()).
        // TODO: Make bulk message bus and bulk size configurable.
        withEagerIdleDetection(config.eagerSignalCollectConvergenceDetection).
        withMessageBusFactory(new BulkAkkaMessageBusFactory[Int, Double](10000, true)).
        withBlockingGraphModificationsSupport(false).
        withStatsReportingInterval(config.heartbeatIntervalInMs).
        withMessageSerialization(serializeMessages).
        //withSchedulerFactory(if (config.asynchronous) new PslSchedulerFactory[Int, Double]() else new Throughput[Int, Double]).
        withEdgeAddedToNonExistentVertexHandlerFactory(consensusHandlerFactory).
        withKryoRegistrations(List(
          "com.signalcollect.admm.ObjectiveValueAggregator$",
          "com.signalcollect.admm.optimizers.SquaredHingeLossOptimizer",
          "breeze.linalg.DenseVector$mcD$sp",
          "com.signalcollect.util.IntDoubleHashMap",
          "com.signalcollect.psl.PslOptimizerWrapper",
          "com.signalcollect.psl.LinearConstraintTerm",
          "com.signalcollect.psl.SquaredHingeLossTerm",
          "com.signalcollect.psl.HingeLossTerm",
          "cern.colt.matrix.tdouble.impl.DenseDoubleMatrix2D",
          "com.signalcollect.psl.ADMMReasoner",
          "com.signalcollect.admm.Wolf$$anonfun$createGraph$1",
          "com.signalcollect.admm.NonExistentConsensusVertexHandlerFactory",
          "com.signalcollect.admm.NonExistentConsensusVertexHandler",
          "com.signalcollect.admm.graph.DummyEdge",
          "com.signalcollect.admm.graph.SubproblemToConsensusSignal",
          "com.signalcollect.admm.graph.ConsensusVertex",
          "com.signalcollect.admm.graph.AsyncConsensusVertex",
          "com.signalcollect.admm.graph.SubproblemVertex",
          "com.signalcollect.admm.graph.AsyncSubproblemVertex",
          "com.signalcollect.MultiAggregator",
          "com.signalcollect.admm.PrimalAggregator$",
          "com.signalcollect.admm.DualAggregator$",
          "com.signalcollect.admm.ConsensusAggregator$",
          "com.signalcollect.admm.PrimalData",
          "com.signalcollect.admm.DualData",
          "akka.actor.PoisonPill$"))
    }
    val graph = graphBuilder.build

    var id = -1
    for (function <- functions) {
      createSubproblem(graph, id, function, config)
      id -= 1
    }
    graph
  }

  def createSubproblem(
    graph: Graph[Int, Double],
    id: Int,
    f: OptimizableFunction,
    config: WolfConfig = new WolfConfig()) = {

    // Debug statement.
    if (id.abs % 10000 == 0) {
      print("*")
    }

    // If possible use the ids contained in the optimizable functions (e.g. converted grounded rules id).
    // Otherwise use the standard id that is incremented at each loop.
    // Potentially allows to parallelize the graph creation in case there are already ids.
    val subId = f.id match {
      case Some(s) => -s
      case None => id
    }
    assert(f.getStepSize == config.stepSize)
    val subproblem = if (config.asynchronous) {
      new AsyncSubproblemVertex(
        subproblemId = subId,
        optimizableFunction = f)
    } else {
      new SubproblemVertex(
        subproblemId = subId,
        optimizableFunction = f)
    }
    for (consensusId <- f.idToIndexMappings) {
      subproblem.addEdge(new DummyEdge(consensusId), graph)
      graph.addEdge(consensusId, new DummyEdge(subId))
    }
    graph.addVertex(subproblem)
  }
}

case object ConsensusAggregator extends ModularAggregationOperation[Option[IntDoubleHashMap]] {
  val neutralElement = None
  def extract(v: Vertex[_, _, _, _]): Option[IntDoubleHashMap] = {
    v match {
      case c: Consensus =>
        val m = new IntDoubleHashMap(initialSize = 4, rehashFraction = 0.5f)
        m.put(c.variableId, c.consensus)
        Some(m)
      case other =>
        None
    }
  }
  def aggregate(
    a: Option[IntDoubleHashMap],
    b: Option[IntDoubleHashMap]): Option[IntDoubleHashMap] = {
    if (a.isEmpty) {
      b
    } else if (b.isEmpty) {
      a
    } else {
      val mA = a.get
      val mB = b.get
      if (mA.size < mB.size) {
        mA.foreach { case (k, v) => mB.put(k, v) }
        Some(mB)
      } else {
        mB.foreach { case (k, v) => mA.put(k, v) }
        Some(mA)
      }
    }
  }
}

case object ObjectiveValueAggregator extends ModularAggregationOperation[Double] {
  val neutralElement = 0.0
  def extract(v: Vertex[_, _, _, _]): Double = {
    v match {
      case s: Subproblem => s.optimizableFunction.evaluateAtEfficient(s.consensusAssignments)
      case other => neutralElement
    }
  }
  def aggregate(
    a: Double,
    b: Double): Double = {
    a + b
  }
}

