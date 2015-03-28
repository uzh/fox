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

object PredicateProperty {
  val validProperties = Set(Functional, Symmetric, PartialFunctional, InverseFunctional, InversePartialFunctional, CompletelyGroundedSets)
  def parse(s: String): PredicateProperty = {
    s match {
      case Functional(_) => Functional
      case Symmetric(_) => Symmetric
      case PartialFunctional(_) => PartialFunctional
      case InverseFunctional(_) => InverseFunctional
      case InversePartialFunctional(_) => InversePartialFunctional
      case CompletelyGroundedSets(_) => CompletelyGroundedSets
      case other => throw new Exception(
        s"Could not parse relation property $other, valid properties are:\n" +
          validProperties.mkString(", "))
    }
  }
}

sealed trait PredicateProperty {
  def unapply(s: String): Option[PredicateProperty] = if (s == toString) Some(PredicateProperty.this) else None
}

case object Functional extends PredicateProperty {
  override val toString = "Functional"
}

case object Symmetric extends PredicateProperty {
  override val toString = "Symmetric"
}

case object PartialFunctional extends PredicateProperty {
  override val toString = "PartialFunctional"
}

case object InverseFunctional extends PredicateProperty {
  override val toString = "InverseFunctional"
}

case object InversePartialFunctional extends PredicateProperty {
  override val toString = "InversePartialFunctional"
}

case object CompletelyGroundedSets extends PredicateProperty {
  override val toString = "CompletelyGroundedSets"
}

// These are not allowed in the parsing, but are used for the boundedness of the grounded predicates.
case object LessOrEqual extends PredicateProperty {
  override val toString = "LessOrEqual"
}

case object GreaterOrEqual extends PredicateProperty {
  override val toString = "GreaterOrEqual"
}