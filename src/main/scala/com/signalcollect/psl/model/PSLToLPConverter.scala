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

import java.io.File
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.psl.Optimizer
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.Grounding
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector

object PSLToLPConverter {

  def toLP(pslString: String): String = {
    val pslData = PslParser.parse(pslString)
    println(s"String parsed.")
    toLP(pslData)
  }

  def toLP(pslFile: File): String = {
    val pslData = PslParser.parse(pslFile)
    println(s"File ${pslFile.getName()} parsed.")
    toLP(pslData)
  }

  def toLP(pslData: ParsedPslFile): String = {
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData)
    println(s"Grounding completed: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")
    toLP(groundedRules, groundedConstraints)
  }

  def toLP(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    val variables = getVariables(rules, constraints)
    val functions = rules.filter(_.definition.weight != Double.MaxValue).map(toLPFunction).mkString(" + ")
    val constraintFunctions = rules.filter(_.definition.weight == Double.MaxValue).map(toLPConstraint).mkString("\n")
    val subjectTo = constraintFunctions + constraints.map(toLPConstraint).mkString("\n") + "\n" + toVariableConstraints(variables)
    s"\nminimize\nobj:${functions} \nsubject to \n${subjectTo}\nbinary\n${variables.mkString(" ")}\nend"
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

  def toLPFunction(rule: GroundedRule): String = {
    val constant = rule.computeConstant
    val coefficientMatrix = rule.computeCoefficientMatrix
    assert(coefficientMatrix.size <= 1, "Not implemented yet for more complex functions with coefficient matrix >1.")
    assert(rule.definition.distanceMeasure != Squared, "Not implemented yet for squared hingeloss functions.")
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + rule)
      return ""
    }
    val zIndices = rule.unboundGroundedPredicates.map(gp => "x" + gp.id)
    s"${rule.definition.weight * coefficientMatrix(0)} ${zIndices(0)}"
  }

  def toLPConstraint(constraint: GroundedConstraint): String = {
    val constant = constraint.computeConstant
    val coefficientMatrix = constraint.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + constraint)
      return ""
    }
    val zIndices = constraint.unboundGroundedPredicates.map(gp => "x" + gp.id)
    val comparator = constraint.computeComparator
    val vector = coefficientMatrix.zipWithIndex.map{ case (c, i) => s"$c ${zIndices(i)}"}
    s"${vector.mkString(" + ")} ${if (comparator == "leq") "<=" else "=="} ${constant}"
  }

  def toLPConstraint(function: GroundedRule): String = {
    val constant = function.computeConstant
    val coefficientMatrix = function.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + function)
      return ""
    }
    val zIndices = function.unboundGroundedPredicates.map(gp => "x" + gp.id)
    val vector = coefficientMatrix.zipWithIndex.map { case (c, i) => s"$c ${zIndices(i)}"}
    s"${vector.mkString(" + ")} <=  ${constant}"
  }

}
