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

package com.signalcollect.psl.model

case class GroundedPredicate(
  id: Int,
  definition: Predicate,
  groundings: List[Individual],
  truthValue: Option[Double],
  lowerBound: Double = 0.0,
  upperBound: Double = 1.0) {

  override def toString() = {
    s"GroundedPredicate $id: ${definition.name}${definition.properties.mkString("[ ", ", ", "]")} ${groundings.mkString("(", ", ", ")")}${
      truthValue match {
        case Some(t) => ": " + t.toString
        case None => ""
      }
    }"
  }
}
