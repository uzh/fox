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

import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.Rule
import com.signalcollect.psl.model.Individual

case class ParsedPslFile(
  classes: Map[String, Set[Individual]],
  predicates: List[Predicate],
  rules: List[Rule],
  facts: List[Fact],
  constants: Set[Individual] = Set.empty) {

  def individualsInFacts = factsWithPredicates.flatMap(_.indsWithClasses).distinct
  def individualsInRules = rulesWithPredicates.flatMap(_.body.flatMap(p => p.individuals)) ++ rulesWithPredicates.flatMap(_.head.flatMap(p => p.individuals))

  def individuals: List[Individual] = {
    val individualsInClasses = classes.map(_._2).flatten
    val allIndividuals = constants.toList ++ individualsInFacts ++ individualsInClasses ++ individualsInRules
    val individualNames = allIndividuals.map(v => v.name).distinct
    val mergedIndividuals = individualNames.map {
      name =>
        val individualsToMerge = allIndividuals.filter(_.name == name)
        val classTypes = individualsToMerge.map(v => v.classTypes).flatten.toSet
        Individual(name, classTypes)
    }
    mergedIndividuals
  }

  def individualsWithoutClass: Set[Individual] = individuals.filter(_.classTypes.isEmpty).toSet

  def individualsByClass: Map[String, Set[Individual]] = {
    // Add individualsInFacts to the proper classes List.
    val classesInFacts = individualsInFacts.flatMap(_.classTypes).distinct
    val addFacts = classesInFacts.map(c => (c, individualsInFacts.filter(i => i.classTypes.contains(c)).toSet)).toMap

    // Add individualsInRules to the proper classes list.
    val classesInRules = individualsInRules.flatMap(_.classTypes).distinct
    val addRules = classesInRules.map(c => (c, individualsInRules.filter(i => i.classTypes.contains(c)).toSet)).toMap

    // For each class add them together.
    val allClasses = classes.map {
      case (classname, inds) =>
        (classname, inds ++ { if (addFacts.contains(classname)) addFacts(classname) else Set() } ++ { if (addRules.contains(classname)) addRules(classname) else Set() })
    }
    allClasses ++ Map("_" -> individuals.toSet)
  }

  val rulesWithPredicates =
    rules.map {
      rule =>
        Rule(rule.id, rule.body.map(mergePredicateInRule(_)), rule.head.map(mergePredicateInRule(_)), rule.distanceMeasure, rule.weight, rule.existentialVars)
    }

  val factsWithPredicates =
    facts.map {
      fact =>
        val predicate = predicates.filter(_.name == fact.name)
        if (predicate.length >= 1) {
          Fact(fact.name, fact.variableGroundings, fact.truthValue, Some(predicate(0)))
        } else {
          println(s"[ERROR] Predicate not declared: $fact.name; will not be grounded properly.")
          fact
        }
    }

  def mergePredicateInRule(pInR: PredicateInRule) = {
    val predicate = predicates.filter(_.name == pInR.name)
    if (predicate.length >= 1) {
      PredicateInRule(pInR.name, pInR.variableOrIndividual, pInR.negated, Some(predicate(0)))
    } else {
      println(s"[ERROR] Predicate not declared: $pInR.name; will not be grounded properly.")
      pInR
    }
  }
}


