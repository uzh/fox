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

case class Rule(
  id: Int, // change in PslParser.
  body: List[PredicateInRule], // implied conjunction
  head: List[PredicateInRule], // implied disjunction
  distanceMeasure: DistanceMeasure,
  weight: Double,
  existentialVars: Set[String] = Set.empty, // Only in head.
  foreachInSetClauseInHead: Set[(String, String, Int, Int, String)] = Set.empty, // Warning: this is not standard Lukasiewicz.
  existsInSetClauseInHead: Set[(String, String, Int, Int, String)] = Set.empty,
  foreachInSetClauseInBody: Set[(String, String, Int, Int, String)] = Set.empty,
  existsInSetClauseInBody: Set[(String, String, Int, Int, String)] = Set.empty // Warning: this is not standard Lukasiewicz.
  ) {
  override def toString = {
    val foreachInHeadString = if (foreachInSetClauseInHead.size > 0) "FOREACH" + foreachInSetClauseInHead.mkString(", ") else ""
    val foreachInBodyString = if (foreachInSetClauseInBody.size > 0) "FOREACH" + foreachInSetClauseInBody.mkString(", ") else ""
    val existInHeadString = if (existsInSetClauseInHead.size > 0) "EXISTS" + existsInSetClauseInHead.mkString(", ") else ""
    val existInBodyString = if (existsInSetClauseInBody.size > 0) "EXISTS" + existsInSetClauseInBody.mkString(", ") else ""
    val conditionsString = body.mkString(" && ")
    val implicationsString = head.mkString(" || ")
    val properties = (weight, distanceMeasure) match {
      case (Double.MaxValue, Squared) => ""
      case (Double.MaxValue, Linear) => s" [$Linear]"
      case (w, Squared) => s" [weight = $w]"
      case (w, Linear) => s" [weight = $w, distanceMeasure = $Linear]"
    }
    s"rule$properties: $foreachInBodyString $existInBodyString $conditionsString => $foreachInHeadString $existInHeadString $implicationsString"
  }

  val allVariables: List[Variable] = {
    val bodyVars = body.flatMap(_.variables)
    val headVars = head.flatMap(_.variables)
    val allVars = bodyVars ++ headVars
    val allVarsNames = allVars.map(v => v.value).distinct
    val mergedVars = allVarsNames.map {
      name =>
        val varsToMerge = allVars.filter(_.value == name)
        val classTypes = varsToMerge.map(v => v.classTypes).flatten.toSet
        Variable(name, classTypes)
    }
    mergedVars
  }

  val variables = allVariables.filter(!_.set)
  val setVariables = allVariables.filter(_.set)

  val partialGroundingNames: List[String] = {
    val bodyInd = body.flatMap(_.individuals)
    val headInd = head.flatMap(_.individuals)
    val allInds = bodyInd ++ headInd
    allInds.map(v => v.value).distinct
  }

  def allPredicatesInRule = body ++ head
}

case class PredicateInRule(
  name: String,
  variableOrIndividual: List[VariableOrIndividual],
  negated: Boolean = false,
  predicate: Option[Predicate] = None) {

  val allVarsOrIndsWithClasses = {
    predicate match {
      case Some(p) =>
        VariableOrIndividualUtils.getVariablesOrIndividualsWithClasses(p, variableOrIndividual)
      case None => variableOrIndividual
    }
  }

  //val varsOrIndsWithClasses = allVarsOrIndsWithClasses.filter(!_.set)

  //val setVarsOrIndsWithClasses = allVarsOrIndsWithClasses.filter(_.set)

  val variables = allVarsOrIndsWithClasses.filter(!_.set).flatMap {
    case v: Variable => Some(v)
    case _ => None
  }

  val individuals = allVarsOrIndsWithClasses.flatMap {
    case i: Individual => Some(i)
    case _ => None
  }

  val singleIndividuals = predicate match {
    case Some(p) =>
      VariableOrIndividualUtils.getVariablesOrIndividualsWithClasses(p, variableOrIndividual, getSingleIndividuals = true).flatMap {
        case i: Individual => Some(i)
        case _ => None
      }
    case None => variableOrIndividual.flatMap {
      case i: Individual => Some(i)
      case _ => None
    }
  }

  override def equals(that: Any) = {
    that match {
      case p: PredicateInRule => p.toString == toString
      case _ => false
    }
  }

  override def toString = s"${if (negated) "!" else ""}$name${allVarsOrIndsWithClasses.mkString("(", ", ", ")")}"
}


