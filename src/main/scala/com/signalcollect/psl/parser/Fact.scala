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

case class Fact(
  name: String,
  variableGroundings: List[Set[Individual]],
  truthValue: Option[Double],
  predicate: Option[Predicate] = None) {

  val groundingsAsSingleIndividuals = {
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

  val indsWithClasses = {
    predicate match {
      case Some(p) => {
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
                val setClass = PslClass(classType.id, true)
                Set(Individual(groundingsAsSingleIndividuals(i).toString, Set(setClass))) ++
                  variableGroundings(i).map { ind => Individual(ind.name, Set(individualClass)) }
              }
            }
        }.flatten
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
    s"grounding$truth: $name${groundingsAsSingleIndividuals.mkString("(", ", ", ")")}"
  }
}
