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

sealed trait VariableOrIndividual {
  def name: String
  def classTypes: Set[PslClass]
  def isVariable: Boolean = {
    this match {
      case i: Individual => false
      case v: Variable => true
    }
  }
  def isIndividual: Boolean = {
    this match {
      case i: Individual => true
      case v: Variable => false
    }
  }
  override def toString = if (!classTypes.isEmpty) { name + ":" + classTypes.mkString(",") } else { name }
  override def equals(that: Any) = {
    that match {
      case v: VariableOrIndividual => v.name == name
      case _ => false
    }
  }
}

object VariableOrIndividual {
  def apply(name: String, classTypes: Set[PslClass] = Set.empty): VariableOrIndividual = {
    if (name(0).isUpper) {
      Variable(name, classTypes)
    } else {
      Individual(name, classTypes)
    }
  }
}

case class Individual(name: String, classTypes: Set[PslClass] = Set.empty) extends VariableOrIndividual {
  val individualsInSet =
    name.stripPrefix("Set(").stripSuffix(")").split(",").map(_.trim()).toSet

  val set = name.startsWith("Set(") && individualsInSet.size > 1

  def isDisjoint(that: Individual) =
    if (set) {
      if (!that.set) {
        !individualsInSet.contains(that.toString)
      } else {
        !individualsInSet.exists(that.individualsInSet.contains(_))
      }
    } else {
      if (!that.set) {
        name != that.name
      } else {
        !that.individualsInSet.contains(this.toString)
      }
    }
  override def equals(that: Any) = {
    that match {
      case v: Individual => v.toString == this.toString
      case _ => false
    }
  }

  override def toString = {
    if (set) { name } else if (!name.startsWith("Set(")) { name } else { individualsInSet.head }

  }

}

case class Variable(name: String, classTypes: Set[PslClass] = Set.empty) extends VariableOrIndividual

