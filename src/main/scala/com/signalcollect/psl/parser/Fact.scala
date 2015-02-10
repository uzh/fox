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

package com.signalcollect.psl.parser

import com.signalcollect.psl.model.Individual
import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.PslClass
import com.signalcollect.psl.model.VariableOrIndividualUtils

case class Fact(
  name: String,
  variableGroundings: List[Set[Individual]],
  truthValue: Option[Double],
  predicate: Option[Predicate] = None,
  minTruthValue: Option[Double] = None,
  maxTruthValue: Option[Double] = None) {

  val groundingsAsSingleIndividuals =
    VariableOrIndividualUtils.groundingsAsSingleIndividuals(variableGroundings)

  val indsWithClasses = {
    predicate match {
      case Some(p) => {
        VariableOrIndividualUtils.getIndividualsWithClasses(p, variableGroundings, groundingsAsSingleIndividuals)
      }
      case None => variableGroundings.flatten
    }
  }

  override def toString = {
    val truth = if (truthValue.isDefined) {
      s" [truthValue = ${truthValue.get}]"
    } else {
      ""
    }
    s"grounding [$truth, $minTruthValue, $maxTruthValue]: $name${groundingsAsSingleIndividuals.mkString("(", ", ", ")")}"
  }
}
