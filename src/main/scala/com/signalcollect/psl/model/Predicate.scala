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

package com.signalcollect.psl.model

case class Predicate(
  name: String,
  classes: List[PslClass] = List.empty,
  properties: Set[PredicateProperty] = Set.empty,
  prior: Option[Double] = None) {

  def arity = classes.length

  override def toString = {
    val propertiesString = if (properties.isEmpty) "" else " " + properties.mkString("[", ", ", "]")
    val positionPlaceholders = classes.map(_.toString).mkString("(", ", ", ")")
    s"relation$propertiesString ${prior.getOrElse("")}: $name$positionPlaceholders"
  }
}

case class PslClass(
  id: String,
  set: Boolean = false,
  minCardinalityOption: Option[Int] = None,
  maxCardinalityOption: Option[Int] = None) {
  
  def maxPossibleCardinality = 10

  override def toString = {
    if (set) {
      if (!minCardinalityOption.isDefined && !maxCardinalityOption.isDefined) {
        s"Set [${id}]"
      } else {
        s"Set (${minCardinalityOption.getOrElse(0)}, ${
          maxCardinalityOption match {
            case Some(s) => s.toString
            case None => "-"
          }
        }) [${id}]"
      }
    } else { id }
  }

  override def equals(that: Any) = {
    that match {
      case v: PslClass => (v.id == id) && (v.set == set) &&
        (v.minCardinalityOption == minCardinalityOption) && (v.maxCardinalityOption == maxCardinalityOption)
      case _ => false
    }
  }
}
