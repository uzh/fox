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

package com.signalcollect.psl

import scala.annotation.tailrec
import com.signalcollect.psl.parser.Fact
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.model._

object Grounding {

  /**
   * Main grounding class.
   * Returns the grounded rules and a map from grounded predicate id => grounded predicate instance.
   */
  def ground(pslData: ParsedPslFile, config: InferencerConfig = InferencerConfig()) = {
    val allPossibleSetsAndIndividuals = generateAllPossibleSetsAsIndividuals(pslData.rulesWithPredicates,
      pslData.individualsByClass, config)
    if (config.verbose) println(s"Generated all classes of individuals: ${allPossibleSetsAndIndividuals.size}.")
    val groundedPredicates = createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts,
      allPossibleSetsAndIndividuals, config)
    if (config.verbose) println(s"Created ${groundedPredicates.size} grounded predicates.")
    // Start by grounding the constraints first, so you can use some of the trivial constraints (e.g. symmetric with only one unbound grounded predicate) to assign
    // values to the grounded predicates before passing them to the rules.
    val (groundedConstraints, updatedGroundedPredicates) = createGroundedConstraints(pslData.predicates,
      groundedPredicates, allPossibleSetsAndIndividuals,
      pslData.rulesWithPredicates.size, config)
    if (config.verbose) println(s"Created ${groundedConstraints.size} grounded constraints.")
    val groundedRules = createGroundedRules(pslData.rulesWithPredicates, updatedGroundedPredicates,
      allPossibleSetsAndIndividuals, groundedConstraints.size + 1, config)
    if (config.verbose) println(s"Created ${groundedRules.size} grounded rules.")
    val idToGpMap = updatedGroundedPredicates.values.map(gp => (gp.id, gp)).toMap
    if (!config.isBounded) {
      val bounds = createGroundedConstraintBounds(updatedGroundedPredicates, groundedRules.size +
        groundedConstraints.size + 1, groundedRules.size + groundedConstraints.size + 1, config.pushBoundsInNodes)
      if (config.verbose) println(s"Created ${bounds.size} bounds.")
      (groundedRules, groundedConstraints ++ bounds, idToGpMap)
    } else {
      (groundedRules, groundedConstraints, idToGpMap)
    }
  }

  /**
   * Generates all possible bindings for the variables in 'variables' according to their classes.
   * Assumption: two variables designate two different individuals,
   * i.e. in votes(A,B) if A is anna, B is not anna.
   * Example: find mappings for A,B and individuals: anna, demo, repub.
   * These are all the valid mappings:
   *  (A->anna, B->demo), (A->anna, B->repub), (A->repub, B->anna), (A->repub, B->demo),
   *  (A->demo, B->repub), (A->demo, B->anna).
   *
   */
  def generateBindings(variables: List[Variable], individuals: Map[(String, Int), Set[Individual]],
    config: InferencerConfig = InferencerConfig()): List[Map[String, Individual]] = {
    if (variables.size < 1) {
      return List.empty
    }

    // For each variable try all the individuals that are in the intersection of the classes it has.
    //     Need a list of List[Map[String, Individual]], each one representing the value of a variable.
    //     e.g. List (Map(A -> anna), Map(A -> sara)) 	    
    val allMappingsList = variables.map {
      variable =>
        val variableIndividuals = if (variable.classTypes.isEmpty) {
          individuals(("_", 1)).map(v => Individual(v.value))
        } else {
          val sets = variable.classTypes.map {
            c =>
              if (c.set) {
                val minCard = c.minCardinalityOption.getOrElse(0)
                val maxCard = c.maxCardinalityOption.getOrElse(c.maxPossibleCardinality)
                val range = minCard to maxCard
                range.flatMap(r => individuals((c.id, r))).toSet
              } else {
                individuals((c.id, 1))
              }
          }.toList
          val intersection = sets.foldLeft(sets(0))(_ & _)
          intersection.map(v => Individual(v.value))
        }
        if (variableIndividuals.isEmpty) {
          return List.empty
        }
        variableIndividuals.map(v => Map[String, Individual](variable.value -> v)).toList
    }

    combineListOfBindingsAndPruneRepeatedIndividuals(allMappingsList)

  }

  def combineListOfBindingsAndPruneRepeatedIndividuals(allMappingsList: List[List[Map[String, Individual]]]) = {
    // Use combine to foldleft the values and get the result.
    val foldedList = allMappingsList.foldLeft(List[Map[String, Individual]]())(combine(_, _))

    // Prune all the mappings that have the same individual in more than two variables.
    // Note: this enforces the fact that if you bind an individual to a variable it cannot be bound again.
    // Example: A-> anna, B-> cannot be anna.
    // This may be not what the user expects, so we may want to make it configurable.
    val prunedList = foldedList.flatMap { mapping =>
      // The empty set is disjoint by definition from anything, even itself.
      val ignoreEmptySet = mapping.filter(_._2.value != "")
      if (ignoreEmptySet.values.toSet.size < ignoreEmptySet.keys.size) {
        None
      } else {
        // Check if there is any variables which is not disjoint from the others.
        val notPairwiseDisjoint = ignoreEmptySet.values.exists(
          a => ignoreEmptySet.values.exists { b =>
            a != b && !b.isDisjoint(a)
          })
        if (notPairwiseDisjoint) {
          None
        } else {
          Some(mapping)
        }
      }
    }
    prunedList
  }

  /**
   * Helper function for combining the generated bindings.
   * Use in the foldLeft onf the final result.
   */
  def combine(thislist: List[Map[String, Individual]], thatlist: List[Map[String, Individual]]): List[Map[String, Individual]] = {
    if (!thislist.isEmpty) {
      // For each of the mappings on the left, e.g. List (Map(A -> anna), Map(A -> sara)) 
      // create a merge with a mapping on the right List( Map(P-> demo), Map(P-> repub))
      // so that List(Map(A-> anna, P-> demo), Map(A-> anna, P-> repub), Map(A -> sara, P-> demo), Map(A->sara, P -> repub))
      val combined = thislist.flatMap {
        // For each of the map, for example Map(A -> anna)
        thismap =>
          // For each of the other map for example Map(P-> demo)
          val joinedList = thatlist.map {
            thatmap =>
              // join them.
              val joinedMap = thismap ++ thatmap
              joinedMap
          }
          joinedList
      }
      combined
    } else { thatlist }
  }

  /**
   * Generate all possible sets as individuals, so that they can be grounded where needed.
   */
  def generateAllPossibleSetsAsIndividuals(rules: List[Rule], individuals: Map[PslClass, Set[Individual]],
    config: InferencerConfig = InferencerConfig()): Map[(String, Int), Set[Individual]] = {
    val individualsMap = individuals.map { case (k, v) => (k.id, 1) -> v }
    // Find all set classes in predicates mentioned in rules.
    val setClasses = rules.flatMap {
      rule =>
        if (config.verbose) println(s"Checking variables in rule: $rule")
        rule.allPredicatesInRule.flatMap {
          _.variables.flatMap { v =>
            v.classTypes.filter(_.set)
          }
        }.distinct
    }.toSet

    if (setClasses.isEmpty) {
      if (config.verbose) println("No variable with set type.")
      individualsMap
    } else {

      if (config.verbose) println("Generating all possible sets of individuals.")
      // Get not set individuals.
      val nonSetIndividuals = individuals.filter(!_._1.set)

      // Group the set classes by their base type and get all the sizes of the sets that we need,
      // based on min and max cardinality.
      val sizesOfSetsBySetClass = setClasses.groupBy(_.id).flatMap {
        case (k, listOfSetClasses) =>
          var ranges = Set.empty[Int]
          listOfSetClasses.map {
            setClass =>
              val minCard = setClass.minCardinalityOption.getOrElse(0)
              val maxCard = setClass.maxCardinalityOption.getOrElse(setClass.maxPossibleCardinality)
              if (!ranges.contains(minCard) && !ranges.contains(maxCard)) {
                val r = minCard to maxCard
                ranges = ranges ++ r
              } else if (!ranges.contains(minCard)) {
                val r = minCard to ranges.min - 1
                ranges = ranges ++ r
              } else if (!ranges.contains(maxCard)) {
                val r = ranges.max + 1 to maxCard
                ranges = ranges ++ r
              }
          }
          Map(k -> ranges.toList.sorted)
      }

      // For each of the set classes, create all possible combinations using the non set individuals.
      val allPossibleSetsAsIndividuals = sizesOfSetsBySetClass.flatMap {
        case (singularTypeName, sizes) =>
          val setClass = PslClass(singularTypeName, true)
          val nonSetIndividualsOfClass = nonSetIndividuals.filter(_._1.id == singularTypeName)
          if (nonSetIndividualsOfClass.size == 1) {
            val relevantIndividuals = nonSetIndividualsOfClass.head._2
            // Subsets creates all possible subsets of a set.
            sizes.flatMap {
              size =>
                val subsetsPerSize = relevantIndividuals.subsets(size).map {
                  subset =>
                    val orderedSubset = subset.toList.sortBy(_.name).toSet
                    Individual(orderedSubset.toString, Set(setClass))
                }.toSet
                Some(Map((setClass.id, size) -> subsetsPerSize))
            }
          } else {
            None
          }
      }.flatten.toMap

      // Return original individuals merged with the new individuals for set classes.
      individualsMap ++ allPossibleSetsAsIndividuals
    }

  }

  def getBinding(varOrInd: VariableOrIndividual, binding: Map[String, Individual]) = varOrInd match {
    case v: Variable if !v.set => binding(v.value)
    case v: Variable if v.set =>
      // If the variable is a set, we have to join the results of all of the variables in the set.
      val setVariables = v.varsOrIndividualsInSet.map(VariableOrIndividual(_)).map {
        case variable: Variable =>
          binding(variable.value).varsOrIndividualsInSet
        case individual: Individual =>
          individual.varsOrIndividualsInSet
      }.flatten.toList.sorted.toSet
      if (setVariables.size == 1) {
        // If there is only one set variable, keep it.
        Individual(setVariables.toString)
      } else {
        // Otherwise, remove the empty set.
        Individual(setVariables.filter(_ != "").toString)
      }
    case i: Individual => Individual(i.value)
  }

  /**
   * Create grounded predicates using the rules.
   * First create all possible grounded predicates using the rules and individuals.
   * This avoids creating grounded predicates that are not used in rules.
   * Then add the truth values that are in the facts and the predicates.
   */
  def createGroundedPredicates(rules: List[Rule], predicates: List[Predicate], facts: List[Fact],
    individuals: Map[(String, Int), Set[Individual]],
    config: InferencerConfig = InferencerConfig()): Map[(String, List[Individual]), GroundedPredicate] = {
    // Collect the truth values in facts.
    val parallelMapOfFacts = if (config.parallelizeGrounding) { facts.par } else { facts }
    val truthValues = parallelMapOfFacts.map { fact =>
      ((fact.name, fact.groundingsAsSingleIndividuals.map(_.value)), (fact.truthValue, fact.minTruthValue, fact.maxTruthValue))
    }.toMap

    // Ground predicates in rules.
    val groundedPredicatesKeys =
      rules.flatMap {
        rule =>
          if (config.verbose) println(s"Creating grounded predicate keys for rule: $rule")
          val bindings = generateBindings(rule.variables, individuals, config)
          val parallelMapOfBindings = if (config.parallelizeGrounding) {
            bindings.par
          } else {
            bindings
          }
          val result = parallelMapOfBindings.flatMap {
            binding =>
              val bodyContribution = rule.body.map(p => (p.predicate.get, p.allVarsOrIndsWithClasses.map {
                getBinding(_, binding)
              }))
              val headContribution = rule.head.map(p => (p.predicate.get, p.allVarsOrIndsWithClasses.map {
                getBinding(_, binding)
              }))
              val totalContribution = bodyContribution ++ headContribution
              totalContribution
          }
          if (config.parallelizeGrounding) {
            result.seq
          } else {
            result
          }
      }.toSet

    //Ground predicates in constraints
    val groundedConstraintPredicatesKeys = predicates.filter(!_.properties.isEmpty).flatMap {
      predicate =>
        if (config.verbose) println(s"Creating grounded constraint keys for predicate: $predicate")
        predicate.properties.flatMap {
          property =>
            property match {
              case Functional | PartialFunctional =>
                val varA = Variable("A", Set(predicate.classes(0)))
                val varB = Variable("B", Set(predicate.classes(1)))
                val bindings = generateBindings(List(varA, varB), individuals, config)
                val parallelMapOfBindings = if (config.parallelizeGrounding) { bindings.par } else { bindings }
                val result = parallelMapOfBindings.flatMap {
                  binding => Some((predicate, List(binding("A"), binding("B"))))
                }
                if (config.parallelizeGrounding) { result.seq } else { result }
              case Symmetric =>
                val varA = Variable("A", predicate.classes.toSet)
                val varB = Variable("B", predicate.classes.toSet)
                val bindings = generateBindings(List(varA, varB), individuals, config)
                val parallelMapOfBindings = if (config.parallelizeGrounding) { bindings.par } else { bindings }
                val result = parallelMapOfBindings.flatMap {
                  binding =>
                    Some((predicate, List(binding("A"), binding("B"))))
                }
                if (config.parallelizeGrounding) { result.seq } else { result }
              case _ => None
            }
        }
    }.toSet

    val allGroundedPredicatesKeys = groundedPredicatesKeys ++ groundedConstraintPredicatesKeys

    // Create the grounded predicates by merging the truth values.
    // groundedPredicateKeys contains a set of (predicate, grounding) pairs.
    // The key is the predicate name and the grounding.
    var id = 0

    // Create the grounded constraint predicates by merging the truth values.
    val groundedPredicates = allGroundedPredicatesKeys.map {
      case (pr, grounding) =>
        val (truthValue, minTruthValue, maxTruthValue) = truthValues.getOrElse((pr.name, grounding.map(_.value)), (None, None, None))
        val gp = GroundedPredicate({ id += 1; id }, pr, grounding, truthValue, minTruthValue.getOrElse(0.0), maxTruthValue.getOrElse(1.0))
        ((pr.name, grounding), gp)
    }.toMap

    if (config.verbose) println(s"Created ${groundedPredicates.size} grounded predicate keys.")

    if (!config.removeSymmetricConstraints) {
      return groundedPredicates
    }

    // Symmetric optimization:
    // For all the symmetric predicates, merge the predicate p(a,b) and p(b,a), keeping only the one with the parameters in alphabetical order.
    // In case there is a truth value, keep it.    
    val unaffectedPredicateKeys = groundedPredicates.filter(_._1._2.length != 2)
    val affectedPredicateKeys = groundedPredicates.filter(_._1._2.length == 2)
    val optimizedPredicateKeys = affectedPredicateKeys.map {
      case ((predicateName, grounding), gp) =>
        if (!gp.definition.properties.contains(Symmetric) || grounding(0).name < grounding(1).name) {
          // Alphabetically ordered, so keep it. (An arbitrary criterion)
          ((predicateName, grounding), gp)
        } else {
          // Drop it, but keep the truth value.
          val newgroundings = List(grounding(1), grounding(0))
          // Get the truthvalue
          val otherGp = affectedPredicateKeys((predicateName, newgroundings))

          gp.truthValue match {
            case None =>
              ((predicateName, newgroundings), otherGp)
            case Some(t) =>
              ((predicateName, newgroundings), GroundedPredicate(otherGp.id, otherGp.definition, newgroundings, Some(t), math.max(otherGp.lowerBound, gp.lowerBound), math.min(otherGp.upperBound, gp.upperBound)))
          }
        }
    }
    unaffectedPredicateKeys ++ optimizedPredicateKeys
  }

  /**
   * Create the new rule based on the existential quantifiers on the right side.
   */

  def createExistentiallyGroundedRule(rule: Rule, individuals: Map[(String, Int), Set[Individual]], config: InferencerConfig) = {
    val existentialVariables = rule.variables.filter(v => rule.existentialVars.contains(v.name))
    //println(s"existentialVariables : $existentialVariables")
    val existentialBindings = generateBindings(existentialVariables, individuals, config)
    // Expand the eventual existential quantified variables in the head to a GP for each existential binding of each predicate in rule.
    if (existentialBindings.length > 0) {
      val newHead = existentialBindings.flatMap {
        // For each possible binding of the existentially qualified variables.
        existBinding =>
          rule.head.map {
            // For each predicate in rule in the head, substitute the affected variables with this binding.
            pInR =>
              val newVars = pInR.variableOrIndividual.map {
                v =>
                  if (existBinding.contains(v.value)) {
                    existBinding(v.value)
                  } else { v }
              }
              PredicateInRule(pInR.name, newVars, pInR.negated, pInR.predicate)
          }
      }.distinct
      Rule(rule.id, rule.body, newHead, rule.distanceMeasure, rule.weight, existentialVars = Set.empty,
        rule.foreachInSetClauseInHead, rule.existsInSetClauseInHead, rule.foreachInSetClauseInBody, rule.existsInSetClauseInBody)
    } else {
      rule
    }
  }

  /**
   * Create the new rule based on the exists in set quantifiers in the head.
   */

  def createExistsInSetInHeadGroundedRule(existsInSetVariablesInHead: Set[(Variable, Variable)], binding: Map[String, Individual],
    rule: Rule, individuals: Map[(String, Int), Set[Individual]], config: InferencerConfig) = {
    val foldedList = generateBindingForIteratorsGivenIterables(existsInSetVariablesInHead, binding, individuals)
    // Expand the eventual existential quantified variables in the head to a GP for each existential binding of each predicate in rule.
    if (foldedList.length > 0) {
      val newHead = foldedList.flatMap {
        // For each possible binding of the existentially qualified variables.
        existBinding =>
          rule.head.map {
            // For each predicate in rule in the head, substitute the affected variables with this binding.
            pInR =>
              val newVars = pInR.variableOrIndividual.map {
                v =>
                  if (existBinding.contains(v.value)) {
                    existBinding(v.value)
                  } else { v }
              }
              PredicateInRule(pInR.name, newVars, pInR.negated, pInR.predicate)
          }
      }.distinct
      val existsInSetRule = Rule(rule.id, rule.body, newHead, rule.distanceMeasure, rule.weight, existentialVars = rule.existentialVars,
        rule.foreachInSetClauseInHead, existsInSetClauseInHead = Set.empty, rule.foreachInSetClauseInBody, rule.existsInSetClauseInBody)
      println(binding)
      println(existsInSetRule)
      existsInSetRule
    } else {
      rule
    }
  }

  /**
   * Given a rule and a set of iterator and iterable names, get the related Variables with the classtypes and
   * restrict the cardinality to 1.
   */
  def getIteratorVariablesInRuleFromNames(rule: Rule, iteratorIterableNames: Set[(String, String)]) = {
    iteratorIterableNames.map {
      case (iterator, iterable) =>
        val iteratorVariable = rule.variables.filter(_.name == iterator).head
        // TODO; both can be sets, in which case V1 represents all of the subsets of V.
        // At the moment we constrain the iterator to a cardinality 1 variable.
        val constrainIteratorVariableCardinality = iteratorVariable.classTypes.map(c => PslClass(c.id))
        val iterableVariable = rule.variables.filter(_.name == iterable).head
        (new Variable(iteratorVariable.name, constrainIteratorVariableCardinality), iterableVariable)
    }
  }

  /**
   * Create the grounded rules using the grounded predicates.
   * We do it in a second time, so we can have a unique id for each grounded predicate.
   */
  def createGroundedRules(rules: List[Rule], groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[(String, Int), Set[Individual]], startingId: Int = 0,
    config: InferencerConfig = InferencerConfig()): List[GroundedRule] = {
    var id = startingId
    rules.flatMap {
      rule =>
        // Existentially quantified vars.
        val newRule = createExistentiallyGroundedRule(rule, individuals, config)

        // For each quantified vars: FOREACH [V1 in V] where V1 is the iterator and V the iterable variable.
        val forEachQuantifiedVariablesInHead = getIteratorVariablesInRuleFromNames(rule, rule.foreachInSetClauseInHead)
        val forEachQuantifiedVariablesInBody = getIteratorVariablesInRuleFromNames(rule, rule.foreachInSetClauseInBody)
        // Exists in quantified vars : EXISTS [V1 in V] where V1 is the iterator and V the iterable variable.
        val existsInSetVariablesInHead = getIteratorVariablesInRuleFromNames(rule, rule.existsInSetClauseInHead)
        val existsInSetVariablesInBody = getIteratorVariablesInRuleFromNames(rule, rule.existsInSetClauseInBody)

        val iteratorVariables = 
          (forEachQuantifiedVariablesInBody ++ forEachQuantifiedVariablesInHead ++ existsInSetVariablesInHead ++ existsInSetVariablesInBody).map(_._1)

        // Normal vars.
        // Treat the rule as a normal rule.
        val normalVars = newRule.variables.filter(!iteratorVariables.contains(_))
        //println(s"normalVars : ${normalVars}")
        val bindings = if (normalVars.size == 0) {
          List(Map.empty[String, Individual])
        } else {
          generateBindings(normalVars, individuals, config)
        }
        bindings.flatMap {
          binding =>
            if (iteratorVariables.size == 0) {
              // Standard execution.
              val groundedBody = newRule.body.map(getGroundedPredicate(groundedPredicates, _, binding)).flatten
              val groundedHead = newRule.head.map(getGroundedPredicate(groundedPredicates, _, binding)).flatten
              val unboundGroundedPredicates = groundedHead.filter(!_.truthValue.isDefined) ::: groundedBody.filter(!_.truthValue.isDefined)
              if (unboundGroundedPredicates.size > 0) {
                List(GroundedRule({ id += 1; id }, newRule, groundedBody, groundedHead))
              } else {
                List.empty
              }
            } else {
              // Bind the FOREACH iterator variables in the body directly.

              // Bind the EXISTS iterator variables in the head directly.
              val existsInSetRule = createExistsInSetInHeadGroundedRule(existsInSetVariablesInHead, binding,
                newRule, individuals, config)

              // Bind the FOREACH iterator variables a posteriori with the individuals bound in the iterable variables.
              val (newId, newGroundedRules) = createNewRuleForeachIteratorInHead(forEachQuantifiedVariablesInHead, binding, existsInSetRule, id,
                groundedPredicates, individuals)
              id = newId
              newGroundedRules
            }
        }
    }
  }

  /**
   * Generate bindings for iterators given a binding of iterables.
   */

  def generateBindingForIteratorsGivenIterables(iteratorIterableSet: Set[(Variable, Variable)], binding: Map[String, Individual], individuals: Map[(String, Int), Set[Individual]]) = {
    val allMappingsList = iteratorIterableSet.map {
      case (iterator, iterable) =>
        val individualsOfSubsets = iterator.classTypes.flatMap {
          c =>
            // TODO: currently only subsets of size 1.
            val subsetsOfIterableBoundVar = binding(iterable.name).varsOrIndividualsInSet.subsets(1)
            val key = (c.id, 1)
            val values = subsetsOfIterableBoundVar.flatMap { v =>
              individuals(key).filter(_.name == v.toString)
            }.toSet
            Map(key -> values)
        }.toMap
        generateBindings(List(iterator), individualsOfSubsets)
    }.toList

    combineListOfBindingsAndPruneRepeatedIndividuals(allMappingsList)
  }

  /**
   * Create a new rule for each iterator variable in head.
   */

  def createNewRuleForeachIteratorInHead(forEachQuantifiedVariables: Set[(Variable, Variable)], binding: Map[String, Individual], newRule: Rule, startingId: Int,
    groundedPredicates: Map[(String, List[Individual]), GroundedPredicate], individuals: Map[(String, Int), Set[Individual]]): (Int, List[GroundedRule]) = {
    var id = startingId
    val foldedList = if (forEachQuantifiedVariables.size == 0) {
        List(Map.empty[String, Individual])
      } else {
        generateBindingForIteratorsGivenIterables(forEachQuantifiedVariables, binding, individuals)
      }

    val newGroundedRules = foldedList.flatMap {
      newBinding =>
        val groundedBody = newRule.body.map(getGroundedPredicate(groundedPredicates, _, newBinding ++ binding)).flatten
        val groundedHead = newRule.head.map(getGroundedPredicate(groundedPredicates, _, newBinding ++ binding)).flatten
        val unboundGroundedPredicates = groundedHead.filter(!_.truthValue.isDefined) ::: groundedBody.filter(!_.truthValue.isDefined)
        if (unboundGroundedPredicates.size > 0) {
          Some(GroundedRule({ id += 1; id }, newRule, groundedBody, groundedHead))
        } else {
          None
        }
    }
    (id, newGroundedRules)
  }

  /**
   * Create the grounded constraints using the grounded predicates.
   * Works only with binary predicates, where functional, partialfunctional and symmetric are defined.
   * We do it in a second time, so we can have a unique id for each grounded predicate.
   */
  def createGroundedConstraints(predicates: List[Predicate], groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[(String, Int), Set[Individual]], startingConstraintId: Int = 0,
    config: InferencerConfig = InferencerConfig()): (List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {
    // The id of the grounded constraint.
    var id = 1
    // The ruleId is the id for each predicate property we are making into a constraint.
    // It is useful when converting to the standard PSL format which has predicate properties as rules.
    var ruleId = startingConstraintId

    // Update the grounded predicates with assignments for the easy cases.
    var currentGroundedPredicates = groundedPredicates

    // Create the grounded constraints based on the predicate properties.
    val groundedConstraints = predicates.filter(!_.properties.isEmpty).flatMap {
      predicate =>
        predicate.properties.flatMap {
          property =>
            val (nextId, con, newGps: Map[(String, List[Individual]), GroundedPredicate]) = property match {
              case Symmetric =>
                if (!config.removeSymmetricConstraints) {
                  createSymmetricConstraints(id, { ruleId += 1; ruleId }, predicate, currentGroundedPredicates, individuals)
                } else {
                  // We have avoided creating symmetric constraints by rewriting all gps (a,b) and (b,a) to a normalized form 
                  (id - 1, List.empty, Map.empty)
                }
              case Functional | PartialFunctional | _ =>
                createFunctionalConstraints(id, { ruleId += 1; ruleId }, property, predicate, currentGroundedPredicates, individuals, config)
            }
            currentGroundedPredicates = currentGroundedPredicates ++ newGps
            id = nextId + 1
            con
        }
    }
    (groundedConstraints, currentGroundedPredicates)
  }

  /**
   * Helper function creating functional/partialFunctional constraints for a given predicate and a list of individuals.
   */
  def createFunctionalConstraints(startingId: Int, ruleId: Int, property: PredicateProperty, predicate: Predicate,
    groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[(String, Int), Set[Individual]],
    config: InferencerConfig = InferencerConfig()): (Int, List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {

    // Functional means that the first individual is the same in all the grounded predicates of the same constraint.
    var id = startingId

    val varA = Variable("A", Set(predicate.classes(0)))
    val varB = Variable("B", Set(predicate.classes(1)))
    val bindings = generateBindings(List(varA, varB), individuals, config)
    val valuesOfA = bindings.map(m => m("A")).distinct

    // For each individual, e.g. "a" create a constraint involving all other individuals.
    val constraintsAndAssignedGroundedPredicates: List[(Option[GroundedConstraint], Option[GroundedPredicate])] = valuesOfA.map {
      valueOfA =>
        val allBindingsWithA = bindings.filter(_("A") == valueOfA)
        val groundedPredicatesWithA = allBindingsWithA.flatMap {
          binding =>
            val key = (predicate.name, List(binding("A"), binding("B")))
            getGroundedPredicate(groundedPredicates, key)
        }
        val constraint = GroundedConstraint({ id += 1; id }, ruleId, property, groundedPredicatesWithA)
        if (!config.pushBoundsInNodes || constraint.computeCoefficientMatrix.size > 1) {
          (Some(constraint), None)
        } else if (constraint.computeCoefficientMatrix.size < 1) {
          // Rewind the id.
          id = id - 1
          (None, None)
        } else {
          val constant = constraint.computeConstant
          val coefficient = constraint.computeCoefficientMatrix(0)
          val gP = constraint.unboundGroundedPredicates(0)
          // Rewind the id.
          id = id - 1
          if (coefficient != 0.0) {
            if (property == Functional) {
              // If the constraint is functional (an equality) we can try to assign the value to the
              // grounded predicate if there is only one numerical value it has to be equal to.
              // println(s"Updated $gP with ${constant/coefficient}.")

              (None, Some(GroundedPredicate(gP.id, gP.definition, gP.groundings, Some(constant / coefficient))))
            } else {
              // println(s"Bound $gP with ${constant/coefficient}.")
              // If the constraint is partial functional (a lesser than or equal), we can exclude certain trivial cases
              // and push them in the bounds.
              val potentialUpperBound = constant / coefficient
              val newUpperBound = math.min(potentialUpperBound, gP.upperBound)
              if (newUpperBound < 0.0) {
                println("[Warning]: There is a constraint which expects the value of a predicate to be lower than 0, we ignore it. ")
                (None, None)
              } else if (newUpperBound == 0.0) {
                // Assign truth value to 0.0.
                (None, Some(GroundedPredicate(gP.id, gP.definition, gP.groundings, Some(0.0))))
              } else {
                (None, Some(GroundedPredicate(gP.id, gP.definition, gP.groundings, gP.truthValue, gP.lowerBound, newUpperBound)))
              }
            }
          } else {
            println("[Warning]: There is a constraint with one unbound predicate and a coefficient matrix with a 0, we ignore it.")
            (None, None)
          }
        }
    }

    val constraints = constraintsAndAssignedGroundedPredicates.flatMap(_._1)
    val reassignedMapOfGps = constraintsAndAssignedGroundedPredicates.flatMap { case (c, p) => if (p.isDefined) { Some(Map((p.get.definition.name, p.get.groundings) -> p.get)) } else { None } }.flatten
    (id, constraints, reassignedMapOfGps.toMap)

  }

  def createGroundedConstraintBounds(groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    startingId: Int = 0, startingConstraintId: Int = 0, pushBoundsInNodes: Boolean = true): List[GroundedConstraint] = {
    var id = startingId
    var ruleId = startingConstraintId

    val unboundedGroundedPredicates = groundedPredicates.values.filter(!_.truthValue.isDefined)
    println(s"Unbounded grounded predicates: ${unboundedGroundedPredicates.size}")

    // Create the grounded constraints of >= 0 and <= 1 for each of the grounded predicates.
    // Keep the grounded predicates that have no truth value (for the others the constraints are useless). 
    val bounds = unboundedGroundedPredicates.map {
      gp =>
        val newLowerBound = math.max(0, math.min(gp.lowerBound, 1.0))
        val newUpperBound = math.max(0, math.min(gp.upperBound, 1.0))
        val leqUpperBound = GroundedConstraint({ id += 1; id }, { ruleId += 1; ruleId }, LessOrEqual, List(gp), Array(1.0), newUpperBound)
        val geqLowerBound = GroundedConstraint({ id += 1; id }, { ruleId }, GreaterOrEqual, List(gp), Array(1.0), newLowerBound)
        List(leqUpperBound, geqLowerBound)
    }.flatten
    bounds.toList
  }

  def getIndividualAsUnionOfBindings(v: Variable, binding: Map[String, Individual]): Individual = {
    val unionOfBindings = v.varsOrIndividualsInSet.flatMap { a =>
      binding.get(a)
    }.filter(_.value != "")
    val notSets = unionOfBindings.filter(!_.set).map(_.toString)
    val sets = unionOfBindings.filter(_.set).flatMap(_.varsOrIndividualsInSet)
    val result = (sets ++ notSets).toList.sorted
    if (result.size > 1) {
      Individual(result.toSet.toString())
    } else if (result.size == 1) {
      Individual(result(0))
    } else {
      Individual("")
    }
  }

  /**
   * Helper class for retrieving the right grounded predicate from the map.
   */
  def getGroundedPredicate(groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    p: PredicateInRule, binding: Map[String, Individual]): Option[GroundedPredicate] = {
    // If the variables is a set of variables, union their bindings.
    val key = (p.name, p.allVarsOrIndsWithClasses.map {
      case v: Variable =>
        if (!v.set) {
          binding(v.value)
        } else {
          getIndividualAsUnionOfBindings(v, binding)
        }
      case i: Individual => Individual(i.value)
    })
    getGroundedPredicate(groundedPredicates, key)
  }

  def getGroundedPredicate(groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    key: (String, List[Individual])): Option[GroundedPredicate] = {
    // For all the symmetric predicates, retrieve the predicate p(a,b) also in case the request is for p(b,a).   
    if (!groundedPredicates.contains(key)) {
      // Check if it's symmetric:
      // Symmetric works only on binary predicates.
      if (key._2.length == 2) {
        // Invert the arguments and try again.
        val newkey = (key._1, List(key._2(1), key._2(0)))
        if (groundedPredicates.contains(newkey)) {
          val retrievedGp = groundedPredicates(newkey)
          // Check if the found predicate is actually symmetric.
          val symmetric = retrievedGp.definition.properties.filter(_ == Symmetric).size
          if (symmetric > 0) {
            return Some(retrievedGp)
          }
        }
      }

      // No grounded predicate.
      println(s"[Warning] Predicate ${key._1} with binding ${key._2} is not in grounded predicates. ")
      return None
    }
    Some(groundedPredicates(key))
  }

  /*
   * Helper function creating symmetric constraints for a given predicate and a list of bindings of individuals.
   * Used only when the hasSymmetricConstraints is true.
   */
  def createSymmetricConstraints(startingId: Int, ruleId: Int, predicate: Predicate, groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[(String, Int), Set[Individual]],
    config: InferencerConfig = InferencerConfig()): (Int, List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {
    // Given all the combinations of individuals, e.g. (a,b), (b,c), (a,c)
    // For each produce the constraint: (a,b) - (b,a) = 0.	  

    // All the possible couples of individuals.
    // All of the current predicate properties refer to binary predicates.
    val varA = Variable("A", predicate.classes.toSet)
    val varB = Variable("B", predicate.classes.toSet)
    val bindings = generateBindings(List(varA, varB), individuals, config)

    // Filter duplicates like Map(A-> a, B-> b) and Map(A -> b, B-> a) and avoid: (a,b) + (b,a) = 0 and (b,a) + (a, b) = 0.
    // Choose the couple that has a lower index in the list.
    val deduplicatedBindings = bindings.zipWithIndex.flatMap {
      case (mapping, i) =>
        val duplicated = bindings.zipWithIndex.map {
          case (other, j) =>
            if (other("A") == mapping("B") && other("B") == mapping("A") && i < j) {
              //keep i
              Some(mapping)
            } else if (other("A") == mapping("B") && other("B") == mapping("A") && i > j) {
              Some(other)
            } else { None }
        }.flatten
        duplicated
    }.distinct

    var id = startingId

    val constraints = deduplicatedBindings.map {
      binding =>
        val keyAB = (predicate.name, List(binding("A"), binding("B")))
        val predicateAB = getGroundedPredicate(groundedPredicates, keyAB).get
        val keyBA = (predicate.name, List(binding("B"), binding("A")))
        val predicateBA = getGroundedPredicate(groundedPredicates, keyBA).get
        GroundedConstraint({ id += 1; id }, ruleId, Symmetric, List(predicateAB, predicateBA))
    }

    (id, constraints, Map.empty)
  }
}
