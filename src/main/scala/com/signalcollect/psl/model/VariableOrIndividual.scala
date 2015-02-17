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

  val numberOfVarsOrIndividualsInSet = varsOrIndividualsInSet.size

  val set = name.startsWith("Set(") && numberOfVarsOrIndividualsInSet > 1

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
    if (set) {
      name
    } else if (!name.startsWith("Set(")) {
      name
    } else {
      varsOrIndividualsInSet.head
    }

  }
  override def hashCode(): Int = {
    value.hashCode()
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
    if (name.stripPrefix("Set(").length() > 0 && name.stripPrefix("Set(")(0).isUpper) {
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

object VariableOrIndividualUtils {

  def getVariablesOrIndividualsWithClasses(p: Predicate, variableGroundings: List[VariableOrIndividual], getSingleIndividuals: Boolean = false): List[VariableOrIndividual] = {
    p.classes.zipWithIndex.map {
      case (classType, i) =>
        if (variableGroundings.length <= i) {
          println(s"Too few arguments for the predicate $p: $variableGroundings")
          List.empty[VariableOrIndividual]
        } else {
          if (classType.id == "_") {
            assert(variableGroundings(i).numberOfVarsOrIndividualsInSet == 1, "Too many variables for an argument that is not a set.")
            List(variableGroundings(i))
          } else if (!classType.set) {
            assert(variableGroundings(i).numberOfVarsOrIndividualsInSet == 1, "Too many variables for an argument that is not a set.")
            List(VariableOrIndividual(variableGroundings(i).toString, Set(classType)))
          } else {
            // The argument is a set of a certain class, so the individual constants are each of that class.
            // Example: symptom (Disease, Set[Symptom]) 
            // symptom(flu, {cough, fever}) => flu: Disease, cough: Symptom, fever: Symptom.
            if (classType.minCardinalityOption.isDefined) {
              assert(variableGroundings(i).numberOfVarsOrIndividualsInSet >= classType.minCardinalityOption.get,
                "Too few variables for an argument set with a minimum cardinality.")
            }
            if (classType.maxCardinalityOption.isDefined) {
              assert(variableGroundings(i).numberOfVarsOrIndividualsInSet <= classType.maxCardinalityOption.get,
                "Too many variables for an argument set with a maximum cardinality.")
            }
            // Get the class of each argument.
            val individualClass = PslClass(classType.id)
            // Get the set class without the cardinalities.
            val isSetClassAnActualSet = classType.minCardinalityOption.getOrElse(0) != 1 || classType.maxCardinalityOption.getOrElse(classType.maxPossibleCardinality) != 1
            val setClass = PslClass(classType.id, isSetClassAnActualSet)
            variableGroundings(i) match {
              case v: Variable =>
                List(VariableOrIndividual(v.toString, Set(classType)))
              case ind: Individual =>
                val individual = Set(VariableOrIndividual(ind.toString, Set(setClass)))
                if (getSingleIndividuals && isSetClassAnActualSet) {
                  individual ++
                    ind.varsOrIndividualsInSet.map { i => VariableOrIndividual(i, Set(individualClass)) }.toSet
                } else {
                  individual
                }

            }

          }
        }
    }.flatten
  }

  def getIndividualsReusingGroundingsAsSingleIndividuals(p: Predicate, variableGroundings: List[Set[Individual]],
    groundingsAsSingleIndividuals: List[Individual], getSingleIndividuals: Boolean = false): List[Individual] = {
    p.classes.zipWithIndex.map {
      case (classType, i) =>
        if (variableGroundings.length <= i) {
          println(s"Too few arguments for the predicate $p: $variableGroundings")
          List.empty
        } else {
          if (classType.id == "_") {
            assert(variableGroundings(i).size == 1, "Too many variables for an argument that is not a set.")
            variableGroundings(i)
          } else if (!classType.set) {
            assert(variableGroundings(i).size == 1, "Too many variables for an argument that is not a set.")
            variableGroundings(i).map { ind => Individual(ind.name, Set(classType)) }
          } else {
            // The argument is a set of a certain class, so the individual constants are each of that class.
            // Example: symptom (Disease, Set[Symptom]) 
            // symptom(flu, {cough, fever}) => flu: Disease, cough: Symptom, fever: Symptom.
            if (classType.minCardinalityOption.isDefined) {
              assert(variableGroundings(i).size >= classType.minCardinalityOption.get,
                "Too few variables for an argument set with a minimum cardinality.")
            }
            if (classType.maxCardinalityOption.isDefined) {
              assert(variableGroundings(i).size <= classType.maxCardinalityOption.get,
                "Too many variables for an argument set with a maximum cardinality.")
            }
            // Get the class of each argument.
            val individualClass = PslClass(classType.id)
            // Get the set class without the cardinalities.
            val isSetClassAnActualSet = classType.minCardinalityOption.getOrElse(0) != 1 || classType.maxCardinalityOption.getOrElse(classType.maxPossibleCardinality) != 1
            val setClass = PslClass(classType.id, isSetClassAnActualSet)
            val individual = Set(Individual(groundingsAsSingleIndividuals(i).toString, Set(setClass)))
            if (getSingleIndividuals && isSetClassAnActualSet) {
              individual ++
                variableGroundings(i).map { ind => Individual(ind.name, Set(individualClass)) }
            } else {
              individual
            }

          }
        }
    }.flatten
  }

  def groundingsAsSingleIndividuals(variableGroundings: List[Set[Individual]]): List[Individual] = {
    variableGroundings.map { singleGrounding =>
      val orderedIndividuals = if (singleGrounding.size > 1) {
        // Before creating a single individual with this set, we order the set of individuals,
        // so we can normalize it.
        singleGrounding.toList.sortBy(_.name).toSet
      } else {
        singleGrounding
      }
      Individual(orderedIndividuals.toString)
    }
  }
}