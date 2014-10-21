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

import com.signalcollect.interfaces.Scheduler
import com.signalcollect.interfaces.SchedulerFactory
import com.signalcollect.interfaces.Worker

class PslSchedulerFactory[@specialized(Int, Long) Id, Signal] extends SchedulerFactory[Id, Signal] {
  def createInstance(worker: Worker[Id, Signal]): Scheduler[Id, Signal] = {
    new PslScheduler[Id, Signal](worker)
  }
  override def toString: String = "PSL Scheduler Factory"
}
