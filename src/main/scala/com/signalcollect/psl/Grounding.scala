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
  def ground(pslData: ParsedPslFile, isBounded: Boolean = true, removeSymmetricConstraints: Boolean = false) = {
    val allPossibleSetsAndIndividuals = generateAllPossibleSetsAsIndividuals(pslData.rulesWithPredicates, pslData.individualsByClass)
    val groundedPredicates = createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts, allPossibleSetsAndIndividuals, removeSymmetricConstraints)
    // Start by grounding the constraints first, so you can use some of the trivial constraints (e.g. symmetric with only one unbound grounded predicate) to assign
    // values to the grounded predicates before passing them to the rules.
    val (groundedConstraints, updatedGroundedPredicates) = createGroundedConstraints(pslData.predicates, groundedPredicates, allPossibleSetsAndIndividuals,
      pslData.rulesWithPredicates.size, removeSymmetricConstraints)
    val groundedRules = createGroundedRules(pslData.rulesWithPredicates, updatedGroundedPredicates, allPossibleSetsAndIndividuals, groundedConstraints.size)
    val idToGpMap = updatedGroundedPredicates.values.map(gp => (gp.id, gp)).toMap
    if (!isBounded) {
      val bounds = createGroundedConstraintBounds(updatedGroundedPredicates, groundedRules.size + groundedConstraints.size, groundedRules.size + groundedConstraints.size)
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
  def generateBindings(variables: List[Variable], individuals: Map[PslClass, Set[Individual]]): List[Map[String, Individual]] = {
    if (variables.size < 1) {
      List.empty
    } else {
      // For each variable try all the individuals that are in the intersection of the classes it has.
      val allMappings = variables.map {
        variable =>
          (variable.value,
            if (variable.classTypes.isEmpty) {
              individuals(PslClass("_")).map(v => Individual(v.value))
            } else {
              val sets = variable.classTypes.map(individuals(_)).toList
              val intersection = sets.foldLeft(sets(0))(_ & _)
              intersection.map(v => Individual(v.value))
            })
      }.toMap

      // Need a list of List[Map[String, Individual]], each one representing the value of a variable.
      // e.g. List (Map(A -> anna), Map(A -> sara)) 	    
      val allMappingsList = allMappings.map {
        case (variableName, variableIndividuals) => {
          val listOfPossibleMappings = variableIndividuals.map(v => Map[String, Individual](variableName -> v)).toList
          listOfPossibleMappings
        }
      }.toList

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
          val notPairwiseDisjoint = ignoreEmptySet.values.exists(a => ignoreEmptySet.values.exists { b =>
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
  def generateAllPossibleSetsAsIndividuals(rules: List[Rule], individuals: Map[PslClass, Set[Individual]]): Map[PslClass, Set[Individual]] = {
    // Find all set classes in predicates mentioned in rules.
    val setClasses = rules.flatMap {
      rule =>
        rule.allPredicatesInRule.flatMap {
          _.variables.flatMap { v =>
            v.classTypes.filter(_.set)
          }.toSet
        }.toSet
    }.toSet

    if (setClasses.isEmpty) {
      return individuals
    }

    // Get not set individuals.
    val nonSetIndividuals = individuals.filter(!_._1.set)

    // For each of the set classes, create all possible combinations using the non set individuals.
    val allPossibleSetsAsIndividuals = setClasses.flatMap { setClass =>
      val nonSetIndividualsOfClass = nonSetIndividuals.filter(_._1.name == setClass.name)
      if (nonSetIndividualsOfClass.size == 1) {
        val relevantIndividuals = nonSetIndividualsOfClass.head._2
        // Subsets creates all possible subsets of a set.
        val allPossibleSetsForClass = relevantIndividuals.subsets.map(
          subset => Individual(subset.toString, Set(setClass))).toSet
        Some(Map(setClass -> allPossibleSetsForClass))
      } else {
        None
      }
    }.flatten.toMap

    // Return original individuals merged with the new individuals for set classes.
    individuals ++ allPossibleSetsAsIndividuals
  }

  /**
   * Create grounded predicates using the rules.
   * First create all possible grounded predicates using the rules and individuals.
   * This avoids creating grounded predicates that are not used in rules.
   * Then add the truth values that are in the facts and the predicates.
   */
  def createGroundedPredicates(rules: List[Rule], predicates: List[Predicate], facts: List[Fact],
    individuals: Map[PslClass, Set[Individual]], removeSymmetricConstraints: Boolean = false): Map[(String, List[Individual]), GroundedPredicate] = {
    // Ground predicates in rules.
    val groundedPredicatesKeys =
      rules.flatMap {
        rule =>
          val bindings = generateBindings(rule.variables, individuals)
          bindings.flatMap {
            binding =>
              val bodyContribution = rule.body.map(p => (p, p.varsOrIndsWithClasses.map {
                case v: Variable => binding(v.value)
                case i: Individual => Individual(i.value)
              }))
              val headContribution = rule.head.map(p => (p, p.varsOrIndsWithClasses.map {
                case v: Variable => binding(v.value)
                case i: Individual => Individual(i.value)
              }))
              val totalContribution = bodyContribution ++ headContribution
              totalContribution
          }
      }.toSet

    //Ground predicates in constraints
    val groundedConstraintPredicatesKeys = predicates.filter(!_.properties.isEmpty).flatMap {
      predicate =>
        predicate.properties.flatMap {
          property =>
            property match {
              case Functional | PartialFunctional =>
                val varA = Variable("A", Set(predicate.classes(0)))
                val varB = Variable("B", Set(predicate.classes(1)))
                val bindings = generateBindings(List(varA, varB), individuals)
                bindings.flatMap {
                  binding => List(Some((predicate, List(binding("A"), binding("B")))))
                }
              case Symmetric =>
                val varA = Variable("A", predicate.classes.toSet)
                val varB = Variable("B", predicate.classes.toSet)
                val bindings = generateBindings(List(varA, varB), individuals)
                bindings.flatMap {
                  binding =>
                    List(
                      Some((predicate, List(binding("A"), binding("B")))),
                      Some((predicate, List(binding("B"), binding("A")))))
                }
              case _ => List(None)
            }
        }
    }.flatten.toSet

    // Collect the truth values in facts.
    val truthValues = {
      for {
        fact <- facts
      } yield ((fact.name, fact.groundingsAsSingleIndividuals.map(_.value)), fact.truthValue)
    }.toMap

    // Create the grounded predicates by merging the truth values.
    // groundedPredicateKeys contains a set of (predicate, grounding) pairs.
    // The key is the predicate name and the grounding.
    var id = 0
    val groundedPredicates = groundedPredicatesKeys.map {
      case (pInR, grounding) =>
        val gp = GroundedPredicate({ id += 1; id }, pInR.predicate.get, grounding,
          truthValues.getOrElse((pInR.name, grounding.map(_.value)), None))
        ((pInR.name, grounding), gp)
    }.toMap

    // Create the grounded constraint predicates by merging the truth values.
    val groundedConstraintPredicates = groundedConstraintPredicatesKeys.map {
      case (pr, grounding) =>
        val gp = GroundedPredicate({ id += 1; id }, pr, grounding,
          truthValues.getOrElse((pr.name, grounding.map(_.value)), None))
        ((pr.name, grounding), gp)
    }.toMap

    // Merge the two maps, overwriting the duplicate values.
    val allPredicateKeys = groundedPredicates ++ groundedConstraintPredicates

    if (!removeSymmetricConstraints) {
      return allPredicateKeys
    }

    // Symmetric optimization:
    // For all the symmetric predicates, merge the predicate p(a,b) and p(b,a), keeping only the one with the parameters in alphabetical order.
    // In case there is a truth value, keep it.    
    val unaffectedPredicateKeys = allPredicateKeys.filter(_._1._2.length != 2)
    val affectedPredicateKeys = allPredicateKeys.filter(_._1._2.length == 2)
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
              ((predicateName, newgroundings), GroundedPredicate(otherGp.id, otherGp.definition, newgroundings, Some(t)))
          }
        }
    }
    unaffectedPredicateKeys ++ optimizedPredicateKeys
  }

  /**
   * Create the grounded rules using the grounded predicates.
   * We do it in a second time, so we can have a unique id for each grounded predicate.
   */
  def createGroundedRules(rules: List[Rule], groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[PslClass, Set[Individual]], startingId: Int = 0): List[GroundedRule] = {
    var id = startingId
    rules.flatMap {
      rule =>
        // Existentially quantified vars.
        val existentialVariables = rule.variables.filter(v => rule.existentialVars.contains(v.name))
        val existentialBindings = generateBindings(existentialVariables, individuals)
        // Expand the eventual existential quantified variables in the head to a GP for each existential binding of each predicate in rule.
        val newHead = if (existentialBindings.length > 0) {
          existentialBindings.flatMap {
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
        } else { rule.head }
        val newRule = Rule(rule.id, rule.body, newHead, rule.distanceMeasure, rule.weight, Set.empty)

        // Normal vars.
        // Treat the rule as a normal rule.
        val bindings = generateBindings(newRule.variables, individuals)
        bindings.map {
          binding =>
            val groundedBody = newRule.body.map(getGroundedPredicate(groundedPredicates, _, binding)).flatten
            val groundedHead = newRule.head.map(getGroundedPredicate(groundedPredicates, _, binding)).flatten
            val unboundGroundedPredicates = groundedHead.filter(!_.truthValue.isDefined) ::: groundedBody.filter(!_.truthValue.isDefined)
            if (unboundGroundedPredicates.size > 0) {
              Some(GroundedRule({ id += 1; id }, newRule, groundedBody, groundedHead))
            } else {
              None
            }
        }
    }.flatten
  }

  /**
   * Create the grounded constraints using the grounded predicates.
   * Works only with binary predicates, where functional, partialfunctional and symmetric are defined.
   * We do it in a second time, so we can have a unique id for each grounded predicate.
   */
  def createGroundedConstraints(predicates: List[Predicate], groundedPredicates: Map[(String, List[Individual]), GroundedPredicate],
    individuals: Map[PslClass, Set[Individual]], startingConstraintId: Int = 0,
    removeSymmetricConstraints: Boolean = false, isBounded: Boolean = true, startingId: Int = 0): (List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {
    // The id of the grounded constraint.
    var id = startingId
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
                if (!removeSymmetricConstraints) {
                  createSymmetricConstraints(id, { ruleId += 1; ruleId }, predicate, currentGroundedPredicates, individuals)
                } else {
                  // We have avoided creating symmetric constraints by rewriting all gps (a,b) and (b,a) to a normalized form 
                  (id - 1, List.empty, Map.empty)
                }
              case Functional | PartialFunctional | _ =>
                createFunctionalConstraints(id, { ruleId += 1; ruleId }, property, predicate, currentGroundedPredicates, individuals, isBounded)
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
    individuals: Map[PslClass, Set[Individual]], isBounded: Boolean): (Int, List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {

    // Functional means that the first individual is the same in all the grounded predicates of the same constraint.
    var id = startingId

    val varA = Variable("A", Set(predicate.classes(0)))
    val varB = Variable("B", Set(predicate.classes(1)))
    val bindings = generateBindings(List(varA, varB), individuals)
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
        if (constraint.computeCoefficientMatrix.size > 1) {
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
                if (isBounded) {
                  (None, Some(GroundedPredicate(gP.id, gP.definition, gP.groundings, gP.truthValue, gP.lowerBound, newUpperBound)))
                } else {
                  (None, Some(GroundedPredicate(gP.id, gP.definition, gP.groundings, gP.truthValue, gP.lowerBound, newUpperBound)))
                }
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
    startingId: Int = 0, startingConstraintId: Int = 0): List[GroundedConstraint] = {
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
    individuals: Map[PslClass, Set[Individual]]): (Int, List[GroundedConstraint], Map[(String, List[Individual]), GroundedPredicate]) = {
    // Given all the combinations of individuals, e.g. (a,b), (b,c), (a,c)
    // For each produce the constraint: (a,b) - (b,a) = 0.	  

    // All the possible couples of individuals.
    // All of the current predicate properties refer to binary predicates.
    val varA = Variable("A", predicate.classes.toSet)
    val varB = Variable("B", predicate.classes.toSet)
    val bindings = generateBindings(List(varA, varB), individuals)

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

    // TODO: Other easy optimization, if it's an equality and there is only one unbounded predicate, assign it.
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
