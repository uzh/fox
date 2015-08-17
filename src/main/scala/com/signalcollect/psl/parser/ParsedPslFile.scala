/*
 *  @author Sara Magliacane
 *
 *  Copyright 2014 VU University Amsterdam
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
import com.signalcollect.psl.model.Variable
import com.signalcollect.psl.model.Squared
import com.signalcollect.psl.model.PslClass

case class ParsedFile(
  explicitlyMentionedIndividualsInClasses: Map[PslClass, Set[Individual]] = Map.empty,
  predicates: List[Predicate] = List.empty,
  rules: List[Rule] = List.empty,
  facts: List[Fact] = List.empty,
  constants: Set[Individual] = Set.empty) {

  def toParsedPslFile(): ParsedPslFile = {
    ParsedPslFile(explicitlyMentionedIndividualsInClasses, predicates, rules, facts, constants)
  }

  def merge(that: ParsedFile): ParsedFile = {
    ParsedFile(explicitlyMentionedIndividualsInClasses ++ that.explicitlyMentionedIndividualsInClasses, predicates ++ that.predicates, rules ++ that.rules,
      facts ++ that.facts, constants ++ that.constants)
  }

}

case class ParsedPslFile(
  explicitlyMentionedIndividualsInClasses: Map[PslClass, Set[Individual]] = Map.empty,
  predicates: List[Predicate] = List.empty,
  rules: List[Rule] = List.empty,
  facts: List[Fact] = List.empty,
  constants: Set[Individual] = Set.empty) {

  def individualsInFacts = factsWithPredicates.flatMap(_.indsWithClasses).distinct
  def individualsInRules = rulesWithPredicates.flatMap(_.body.flatMap(p => p.singleIndividuals)) ++
    rulesWithPredicates.flatMap(_.head.flatMap(p => p.singleIndividuals))

  def individuals: List[Individual] = {
    val individualsInClasses = explicitlyMentionedIndividualsInClasses.map(_._2).flatten
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

  def classes: Set[PslClass] = {
    val classesInFacts = individualsInFacts.flatMap(_.classTypes).distinct
    val classesInRules = individualsInRules.flatMap(_.classTypes).distinct
    (explicitlyMentionedIndividualsInClasses.keys.toList ++ classesInFacts ++ classesInRules ++ List(PslClass("_"))).toSet
  }

  def individualsWithoutClass: Set[Individual] = individuals.filter(_.classTypes.isEmpty).toSet

  //  def individualsByClass: Map[PslClass, Set[Individual]] = {
  //    // Add individualsInFacts to the proper classes List.
  //    val classesInFacts = individualsInFacts.flatMap(_.classTypes).distinct
  //    val classesAndIndividualsInFacts = classesInFacts.map(c => (c, individualsInFacts.filter(i => i.classTypes.contains(c)).toSet)).toMap
  //
  //    // Add individualsInRules to the proper classes list.
  //    val classesInRules = individualsInRules.flatMap(_.classTypes).distinct
  //    val classesAndIndividualsInRules = classesInRules.map(c => (c, individualsInRules.filter(i => i.classTypes.contains(c)).toSet)).toMap
  //
  //    val classes = explicitlyMentionedIndividualsInClasses.keys.toList ++ classesInFacts ++ classesInRules
  //
  //    // For each class add them together.
  //    val allClasses = classes.map {
  //      case classname =>
  //        (classname, explicitlyMentionedIndividualsInClasses.getOrElse(classname, Set.empty) ++
  //          classesAndIndividualsInFacts.getOrElse(classname, Set.empty) ++
  //          classesAndIndividualsInRules.getOrElse(classname, Set.empty))
  //    }.toMap
  //    val allIndividualsByClass = allClasses ++ Map(PslClass("_") -> individuals.toSet)
  //    allIndividualsByClass
  //  }

  def individualsByClassAndCardinality: Map[(PslClass, Int), Set[Individual]] = {
    classes.map {
      c =>
        val individualsInFactsMatchingClass = if (c.id == "_") {
          individuals.groupBy(_.numberOfVarsOrIndividualsInSet)
        } else {
          individuals.filter(i => i.classTypes.contains(c)).groupBy(_.numberOfVarsOrIndividualsInSet)
        }
        individualsInFactsMatchingClass.map { case (card, inds) => ((c, card), inds.toSet) }.toMap
    }.fold(Map.empty)(_ ++ _)
  }

  val rulesWithPredicates = {
    val standardRulesWithPredicates = rules.map {
      rule =>
        Rule(rule.id, rule.body.map(mergePredicateInRule(_)),
          rule.head.map(mergePredicateInRule(_)), rule.distanceMeasure, rule.weight, rule.existentialVars,
          rule.foreachInSetClauseInHead, rule.existsInSetClauseInHead, rule.foreachInSetClauseInBody, rule.existsInSetClauseInBody)
    }
    // Create rules for predicates that have prior values defined.
    val priorRules = predicates.filter(_.prior.isDefined).flatMap {
      predicate =>
        val predicatePrior = predicate.prior.get
        var i = 0;
        val variables = List.fill(predicate.arity) { i = i + 1; Variable("A" + i) }
        val pInR = PredicateInRule(name = predicate.name, variableOrIndividual = variables, negated = false, predicate = Some(predicate))
        val pInRNegated = PredicateInRule(name = predicate.name, variableOrIndividual = variables, negated = true, predicate = Some(predicate))
        val weightTrue = 0.01
        val rule = Rule(0, List.empty, List(pInR), Squared, weightTrue)
        val ruleNegated = if (predicatePrior != 0.0) {
          Rule(0, List.empty, List(pInRNegated), Squared, weightTrue / predicate.prior.get - weightTrue)
        } else {
          Rule(0, List.empty, List(pInRNegated), Squared, weightTrue)
        }
        if (predicatePrior == 0.0) {
          List(ruleNegated)
        } else if (predicatePrior == 1.0) {
          List(rule)
        } else {
          List(rule, ruleNegated)
        }

    }
    standardRulesWithPredicates ++ priorRules
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