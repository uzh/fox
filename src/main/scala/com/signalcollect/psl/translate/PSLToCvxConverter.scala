/*
 *  @author Sara Magliacane
 *  @author Philip Stutz
 *
 *  Copyright 2013-2015 University of Zurich & VU University Amsterdam
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

package com.signalcollect.psl.translate

import java.io.File
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.psl.Optimizer
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedConstraint
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.Squared
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.Grounding
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector

object PSLToCvxConverter {

  def toCvx(pslString: String): (String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslString)
    println(s"String parsed.")
    toCvx(pslData)
  }

  def toCvx(pslFile: File): (String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslFile)
    println(s"File ${pslFile.getName()} parsed.")
    toCvx(pslData)
  }

  def toCvx(pslData: ParsedPslFile): (String, Map[Int, GroundedPredicate]) = {
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData)
    println(s"Grounding completed: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")
    (toCvx(groundedRules, groundedConstraints), idToGpMap)
  }

  def toCvx(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    val variables = getVariables(rules, constraints)
    val functions = rules.filter(_.definition.weight != Double.MaxValue).map(toCvxFunction).mkString(" + ")
    val constraintFunctions = rules.filter(_.definition.weight == Double.MaxValue).map(toCvxConstraint).mkString("\n")
    val subjectTo = constraintFunctions + constraints.map(toCvxConstraint).mkString("\n") + "\n" + toVariableConstraints(variables)
    s"cvx_begin\nvariables ${variables.mkString(" ")}\nminimize ${functions} \nsubject to \n${subjectTo}\ncvx_end"
  }

  def getVariables(rules: List[GroundedRule], constraints: List[GroundedConstraint]) = {
    val variables =
      (rules ++ constraints).map(rule => rule.unboundGroundedPredicates.map(gp => "x" + gp.id))
    variables.flatten.toSet
  }

  def toVariables(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    getVariables(rules, constraints).mkString(" ")
  }

  def toVariableConstraints(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    toVariableConstraints(getVariables(rules, constraints))
  }

  def toVariableConstraints(variables: Set[String]): String = {
    variables.map(v => s"${v} >= 0\n${v} <= 1").mkString("\n")
  }

  def toCvxFunction(rule: GroundedRule): String = {
    val constant = rule.computeConstant
    val coefficientMatrix = rule.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + rule)
      return ""
    }
    val zIndices = rule.unboundGroundedPredicates.map(gp => "x" + gp.id).mkString("[", " ", "]")
    if (rule.definition.distanceMeasure == Squared) {
      // pow_pos(x, p) = max(0, x)^p
      s"${rule.definition.weight} * pow_pos(${coefficientMatrix.mkString("[", " ", "]")} * ${zIndices}' - ${constant}, 2)"
    } else {
      s"${rule.definition.weight} * max (0, ${coefficientMatrix.mkString("[", " ", "]")} * ${zIndices}' - ${constant})"
    }
  }

  def toCvxConstraint(constraint: GroundedConstraint): String = {
    val constant = constraint.computeConstant
    val coefficientMatrix = constraint.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + constraint)
      return ""
    }
    val zIndices = constraint.unboundGroundedPredicates.map(gp => "x" + gp.id).mkString("[", " ", "]")
    val comparator = constraint.computeComparator
    s"${coefficientMatrix.mkString("[", " ", "]")} * ${zIndices}' ${if (comparator == "leq") "<=" else "=="} ${constant}"
  }

  def toCvxConstraint(function: GroundedRule): String = {
    val constant = function.computeConstant
    val coefficientMatrix = function.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + function)
      return ""
    }
    val zIndices = function.unboundGroundedPredicates.map(gp => "x" + gp.id).mkString("[", " ", "]")
    s"${coefficientMatrix.mkString("[", " ", "]")} * ${zIndices}' <=  ${constant}"
  }

}
