/*
 *  @author Sara Magliacane
 *  @author Philip Stutz
 *
 *  Copyright 2013-2015 University of Zurich & VU University Amsterdam
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
import com.signalcollect.GraphEditor
import com.signalcollect.Vertex
import com.signalcollect.admm.graph.AsyncConsensusVertex
import com.signalcollect.admm.graph.AsyncSubproblemVertex
import com.signalcollect.admm.graph.Consensus
import com.signalcollect.admm.graph.ConsensusVertex
import com.signalcollect.admm.graph.DummyEdge
import com.signalcollect.admm.graph.LazyConsensusVertex
import com.signalcollect.admm.graph.LazySubproblemVertex
import com.signalcollect.admm.graph.Subproblem
import com.signalcollect.admm.graph.SubproblemVertex
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.admm.utils.Timer
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.configuration.TerminationReason
import com.signalcollect.factory.messagebus.BulkAkkaMessageBusFactory
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandler
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandlerFactory
import com.signalcollect.interfaces.ModularAggregationOperation
import com.signalcollect.util.IntDoubleHashMap


import akka.actor.ActorRef

case class ProblemSolution(
  stats: Option[ExecutionInformation[Int, Double]],
  results: IntDoubleHashMap,
  convergence: Option[AbstractGlobalAdmmConvergenceDetection] = None,
  graphLoadingTime: Long,
  inferenceTime: Long,
  resultAggregationTime: Long) // in case we have local convergence this is None.

case class WolfConfig(
  asynchronous: Boolean,
  lazyThreshold: Option[Double], // Absolute threshold for lazy signalling.
  globalConvergenceDetection: Option[Int], // Detect global convergence every 2 S/C steps by default. Note: should be a multiple of 2 for making sure convergence works.
  absoluteEpsilon: Double,
  relativeEpsilon: Double,
  objectiveLoggingEnabled: Boolean,
  maxIterations: Int, // maximum number of iterations.
  timeLimit: Option[Long],
  stepSize: Double,
  isBounded: Boolean,
  serializeMessages: Boolean,
  eagerSignalCollectConvergenceDetection: Boolean,
  heartbeatIntervalInMs: Int)

case class NonExistentConsensusVertexHandlerFactory(
  asynchronous: Boolean, // If the execution is asynchronous.
  initialState: Double, // Initial value for the consensus variable.
  isBounded: Boolean, // Use bounding (cutoff below 0 and above 1).
  lazyThreshold: Option[Double], // Only send values that have changed.
  boundsOnConsensusVars: Map[Int, (Double, Double)] = Map.empty) // Push trivial bounds inside the nodes.
  extends EdgeAddedToNonExistentVertexHandlerFactory[Int, Double] {
  def createInstance: EdgeAddedToNonExistentVertexHandler[Int, Double] =
    new NonExistentConsensusVertexHandler(asynchronous, initialState, isBounded, lazyThreshold, boundsOnConsensusVars)
  override def toString = "NoneExistentConsensusVertexFactory"
}

case class NonExistentConsensusVertexHandler(
  asynchronous: Boolean, // If the execution is asynchronous.
  initialState: Double, // Initial value for the consensus variable.
  isBounded: Boolean, // Use bounding (cutoff below 0 and above 1).
  lazyThreshold: Option[Double], // Only continue if a value changed by more than the threshold.
  boundsOnConsensusVars: Map[Int, (Double, Double)] = Map.empty) // Push trivial bounds inside the nodes.
  extends EdgeAddedToNonExistentVertexHandler[Int, Double] {
  def handleImpossibleEdgeAddition(edge: Edge[Int], vertexId: Int, graphEditor: GraphEditor[Int, Double]): Option[Vertex[Int, _, Int, Double]] = {
    val (lowerBound, upperBound) =
      boundsOnConsensusVars.getOrElse(vertexId, (0.0, 1.0))
    if (asynchronous) {
      if (lazyThreshold.isDefined) {
        println("Asynchronous inferencing cannot be combined with lazy inferencing, lazy setting is being ignored.")
      }
      Some(new AsyncConsensusVertex(
        variableId = vertexId,
        initialState = initialState,
        isBounded = isBounded,
        lowerBound,
        upperBound))
    } else {
      if (lazyThreshold.isDefined) {
        Some(new LazyConsensusVertex(
          variableId = vertexId,
          initialState = initialState,
          isBounded = isBounded,
          lowerBound,
          upperBound))
      } else {
        Some(new ConsensusVertex(
          variableId = vertexId,
          initialState = initialState,
          isBounded = isBounded,
          lowerBound,
          upperBound))
      }
    }
  }
}

// Solves problems.
object Wolf {
  def solveProblem(
    functions: Traversable[OptimizableFunction],
    nodeActors: Option[Array[ActorRef]] = None,
    config: WolfConfig,
    boundsOnConsensusVars: Map[Int, (Double, Double)] = Map.empty): ProblemSolution = {
    if (config.maxIterations > 0) {
      val (graph, graphLoadingTime) = Timer.time {
        createGraph(functions, nodeActors, config, config.serializeMessages, boundsOnConsensusVars)
      }
      try {
        val ((stats, convergence), inferenceTime) = Timer.time {
          println(s"ADMM graph creation completed in $graphLoadingTime ms.\nStarting inference.")
          val baseExecutionConfig = ExecutionConfiguration[Int, Double]().
            withExecutionMode(if (config.asynchronous) ExecutionMode.PureAsynchronous else ExecutionMode.Synchronous).
            withStepsLimit(config.maxIterations)
          val executionConfig = if (config.timeLimit.isDefined) {
            baseExecutionConfig.withTimeLimit(config.timeLimit.get)
          } else { baseExecutionConfig }
          if (config.globalConvergenceDetection.isDefined) {
            // Global convergence case:
            val globalConvergence = if (config.objectiveLoggingEnabled) {
              new GlobalAdmmConvergenceDetection(
                absoluteEpsilon = config.absoluteEpsilon,
                relativeEpsilon = config.relativeEpsilon,
                checkingInterval = config.globalConvergenceDetection.get,
                aggregationInterval = if (config.asynchronous) 500 else 1 // every iteration for sync, every second for async.
                ) with DebugLoggingConvergenceDetection
            } else {
              GlobalAdmmConvergenceDetection(
                absoluteEpsilon = config.absoluteEpsilon,
                relativeEpsilon = config.relativeEpsilon,
                checkingInterval = config.globalConvergenceDetection.get,
                aggregationInterval = if (config.asynchronous) 500 else 1 // every iteration for sync, every second for async.
                )
            }
            val stats = graph.execute(executionConfig.withGlobalTerminationDetection(globalConvergence))
            (stats, Some(globalConvergence))
          } else {
            val stats = graph.execute(executionConfig)
            (stats, None)
          }
        }
        val (results, resultAggregationTime) = Timer.time {
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
          val resultMap = graph.aggregate(ConsensusAggregator)
          resultMap.getOrElse(new IntDoubleHashMap(initialSize = 1, rehashFraction = 0.5f))
        }
        val solution = ProblemSolution(
          stats = Some(stats),
          results = results,
          convergence = convergence,
          graphLoadingTime = graphLoadingTime,
          inferenceTime = inferenceTime,
          resultAggregationTime = resultAggregationTime)
        solution
      } finally {
        graph.shutdown
      }
    } else {
      // maxIterations <= 0.
      ProblemSolution(
        stats = None,
        results = new IntDoubleHashMap(),
        convergence = None,
        graphLoadingTime = 0,
        inferenceTime = 0,
        resultAggregationTime = 0)
    }
  }

  //Convention: subproblems have negative ids.
  def createGraph(
    functions: Traversable[OptimizableFunction],
    nodeActors: Option[Array[ActorRef]] = None,
    config: WolfConfig,
    serializeMessages: Boolean = false,
    boundsOnConsensusVars: Map[Int, (Double, Double)] = Map.empty): Graph[Int, Double] = {
    //println(s"Creating the ADMM graph ...")
    // Use node actors with graph builder, if they have been passed.
    val consensusHandlerFactory = new NonExistentConsensusVertexHandlerFactory(
      asynchronous = config.asynchronous, // If the execution is asynchronous.
      initialState = 0.0, // Initial value for the consensus variable.
      isBounded = config.isBounded, // Use bounding (cutoff below 0 and above 1) .
      lazyThreshold = config.lazyThreshold, // Only send values that have changed.
      boundsOnConsensusVars)
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
          "com.signalcollect.admm.optimizers.HingeLossOptimizer",
          "com.signalcollect.admm.optimizers.LinearLossOptimizer",
          "com.signalcollect.admm.optimizers.SquaredLossOptimizer",
          "com.signalcollect.admm.optimizers.LinearConstraintOptimizer",
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
          "com.signalcollect.admm.graph.SubproblemVertex",
          "com.signalcollect.admm.graph.AsyncConsensusVertex",
          "com.signalcollect.admm.graph.AsyncSubproblemVertex",
          "com.signalcollect.admm.graph.LazyConsensusVertex",
          "com.signalcollect.admm.graph.LazySubproblemVertex",
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
    config: WolfConfig) = {

    // Debug statement.
    //if (id.abs % 10000 == 0) print("*")

    // If possible use the ids contained in the optimizable functions (e.g. converted grounded rules id).
    // Otherwise use the standard id that is incremented at each loop.
    // Potentially allows to parallelize the graph creation in case there are already ids.
    val subId = f.id match {
      case Some(s) => -s
      case None => id
    }
    assert(f.getStepSize == config.stepSize)
    val subproblem = if (config.asynchronous) {
      if (config.lazyThreshold.isDefined) {
        println("Asynchronous inferencing cannot be combined with lazy inferencing, lazy setting is being ignored.")
      }
      new AsyncSubproblemVertex(
        subproblemId = subId,
        optimizableFunction = f)
    } else {
      if (config.lazyThreshold.isDefined) {
        new LazySubproblemVertex(
          subproblemId = subId,
          optimizableFunction = f,
          absoluteSignallingThreshold = config.lazyThreshold.get)
      } else {
        new SubproblemVertex(
          subproblemId = subId,
          optimizableFunction = f)
      }
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

