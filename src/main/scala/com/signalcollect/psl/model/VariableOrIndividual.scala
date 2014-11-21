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

  val varsOrIndividualsInSet =
    name.stripPrefix("Set(").stripSuffix(")").split(",").map(_.trim()).toSet

  val set = name.startsWith("Set(") && varsOrIndividualsInSet.size > 1

  val value = this.toString

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
  override def toString = {
    if (set) { name } else if (!name.startsWith("Set(")) { name } else { varsOrIndividualsInSet.head }

  }
  override def equals(that: Any) = {
    that match {
      case v: VariableOrIndividual => v.value == value
      case _ => false
    }
  }
}

object VariableOrIndividual {
  def apply(name: String, classTypes: Set[PslClass] = Set.empty): VariableOrIndividual = {
    if (name.stripPrefix("Set(")(0).isUpper) {
      Variable(name, classTypes)
    } else {
      Individual(name, classTypes)
    }
  }
}

case class Individual(name: String, classTypes: Set[PslClass] = Set.empty) extends VariableOrIndividual {

  def isDisjoint(that: Individual) =
    if (set) {
      if (!that.set) {
        !varsOrIndividualsInSet.contains(that.toString)
      } else {
        !varsOrIndividualsInSet.exists(that.varsOrIndividualsInSet.contains(_))
      }
    } else {
      if (!that.set) {
        name != that.name
      } else {
        !that.varsOrIndividualsInSet.contains(this.toString)
      }
    }
}

case class Variable(name: String, classTypes: Set[PslClass] = Set.empty) extends VariableOrIndividual
