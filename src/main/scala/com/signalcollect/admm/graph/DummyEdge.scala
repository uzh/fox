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

import com.signalcollect._
import com.signalcollect.interfaces._

case class DummyEdge(val targetId: Int) extends Edge[Int] {
  override def id: EdgeId[_] = throw new UnsupportedOperationException
  override def sourceId: Any = throw new UnsupportedOperationException
  override def source: Source = throw new UnsupportedOperationException
  override def onAttach(source: Vertex[_, _, _, _], graphEditor: GraphEditor[Any, Any]) = throw new UnsupportedOperationException
  override def weight: Double = 1
  override def toString = s"DummyEdge(targetId=$targetId)"
  override def hashCode = throw new UnsupportedOperationException
  override def equals(other: Any): Boolean = throw new UnsupportedOperationException
  override def executeSignalOperation(sourceVertex: Vertex[_, _, _, _], graphEditor: GraphEditor[Any, Any]) = throw new UnsupportedOperationException
}
