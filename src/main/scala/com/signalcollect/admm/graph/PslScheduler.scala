/*
 *  @author Philip Stutz
 *
 *  Copyright 2014 University of Zurich
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

import com.signalcollect.Vertex
import com.signalcollect.interfaces.Scheduler
import com.signalcollect.interfaces.Worker

/**
 * Hybrid that does the batching of the throughput schduler and the immediate signalling after collecting of the low-latency scheduler.
 */
class PslScheduler[Id, Signal](
  w: Worker[Id, Signal],
  val batchSize: Int = 10000)
  extends Scheduler[Id, Signal](w) {

  override def executeOperations(systemOverloaded: Boolean) {
    if (!worker.vertexStore.toCollect.isEmpty) {
      val collected = worker.vertexStore.toCollect.process(
        vertex => {
          worker.executeCollectOperationOfVertex(vertex, addToSignal = false)
          if (vertex.scoreSignal > worker.signalThreshold) {
            if (systemOverloaded) {
              worker.vertexStore.toSignal.put(vertex)
            } else {
              worker.executeSignalOperationOfVertex(vertex)
            }
          }
        }, Some(batchSize))
      worker.messageBusFlushed = false
    }
    if (!systemOverloaded && !worker.vertexStore.toSignal.isEmpty) {
      worker.vertexStore.toSignal.process(worker.executeSignalOperationOfVertex(_), Some(batchSize))
      worker.messageBusFlushed = false
    }
  }

  override def handleCollectOnDelivery(v: Vertex[Id, _, Id, Signal]) {
    worker.executeSignalOperationOfVertex(v)
  }

}
