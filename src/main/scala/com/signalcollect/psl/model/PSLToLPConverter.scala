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
import java.io.FileWriter
import java.io.FileReader
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.psl.Optimizer
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.Grounding
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.Reader
import scala.annotation.tailrec

import scala.sys.process._

object PSLToLPConverter {
  def solve(pslString: String, isBinary: Boolean): Map[GroundedPredicate, Double] = {
    val (translatedProblem, idToGpMap) = toLP(pslString, isBinary)
    solve(translatedProblem, idToGpMap, isBinary)
  }
  def solve(pslFile: File, isBinary: Boolean): Map[GroundedPredicate, Double] = {
    val (translatedProblem, idToGpMap) = toLP(pslFile, isBinary)
    solve(translatedProblem, idToGpMap, isBinary)
  }

  def solve(pslData: ParsedPslFile, isBinary: Boolean): Map[GroundedPredicate, Double] = {
    val (translatedProblem, idToGpMap) = toLP(pslData, isBinary)
    solve(translatedProblem, idToGpMap, isBinary)
  }

  def solve(translatedProblem: String, idToGpMap: Map[Int, GroundedPredicate], isBinary: Boolean): Map[GroundedPredicate, Double] = {
    val writer = new FileWriter("temp-mosek-translation.lp")
    writer.append(translatedProblem)
    writer.close()
    val mosekCommand = "mosek temp-mosek-translation.lp"
    val mosekOutput = mosekCommand.!!
    val mosekResult = if (isBinary) {
      LpResultParser.parse(new File("temp-mosek-translation.int"))
    } else {
      LpResultParser.parse(new File("temp-mosek-translation.sol"))
    }
    mosekResult.map { case (id, value) => (idToGpMap(id), value) }
  }

  def printSelectedResults(mosekResult: Map[GroundedPredicate, Double], queryList: List[String] = List.empty, outputType: String = "inference",
    printBinary: Boolean = false): String = {
    if (queryList.isEmpty) {
      mosekResult.mkString("\n")
    } else {
      var s = ""
      mosekResult.filter { case (gp, value) => queryList.contains(gp.definition.name) }.foreach {
        case result =>
          val factName = if (outputType == "shortInference" || outputType == "onlyTrueFacts") {
            s"""${result._1.definition.name}${result._1.groundings.mkString("(", ",", ")")}"""
          } else {
            result._1.toString
          }
          val truthValue = if (printBinary) {
            if (result._2 > 0.5) 1.0 else 0.0
          } else {
            result._2
          }

          if (outputType == "onlyTrueFacts" && truthValue > 0.5) {
            if (s == "") {
              s += "$factName"
            } else {
              s += ", $factName"
            }
          } else if (outputType != "onlyTrueFacts") {
            s += "\n$factName -> $truthValue"
          } else {
            // Do nothing.
          }
      }
      s
    }
  }
  def toLP(pslString: String, isBinary: Boolean): (String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslString)
    toLP(pslData, isBinary)
  }

  def toLP(pslFile: File, isBinary: Boolean): (String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslFile)
    println(s"File ${pslFile.getName()} parsed.")
    toLP(pslData, isBinary)
  }

  def toLP(pslData: ParsedPslFile, isBinary: Boolean): (String, Map[Int, GroundedPredicate]) = {
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData)
    println(s"Grounding completed: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")
    (toLP(groundedRules, groundedConstraints, isBinary), idToGpMap)
  }

  def toLP(rules: List[GroundedRule], constraints: List[GroundedConstraint], isBinary: Boolean): String = {
    val variables = getVariables(rules, constraints)
    val functions = rules.filter(_.definition.weight != Double.MaxValue).map(toLPFunction).mkString(" ")
    val constraintFunctions = rules.filter(_.definition.weight == Double.MaxValue).map(toLPConstraint).mkString("\n")
    val subjectTo = constraintFunctions + constraints.map(toLPConstraint).mkString("\n")
    if (isBinary) {
      s"\nminimize\nobj: ${functions} \nsubject to\n${subjectTo}\nbinary\n${variables.mkString(" ")}\nend"
    } else {
      val variableBounds = toVariableConstraints(variables)
      s"\nminimize\nobj: ${functions} \nsubject to\n${subjectTo}\nbounds\n${variableBounds}\nend"
    }
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
    variables.map(v => s" 0 <= ${v} <= 1").mkString("\n")
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
    val c = rule.definition.weight * coefficientMatrix(0)
    s"${if (c > 0) s" + $c" else s" - ${-c}"} ${zIndices(0)}"
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
    val vector = coefficientMatrix.zipWithIndex.map { case (c, i) => s"${if (c > 0) s" + $c" else s" - ${-c}"} ${zIndices(i)}" }
    s"${vector.mkString(" ")} ${if (comparator == "leq") "<=" else "=="} ${constant}"
  }

  def toLPConstraint(function: GroundedRule): String = {
    val constant = function.computeConstant
    val coefficientMatrix = function.computeCoefficientMatrix
    if (coefficientMatrix.size < 1) {
      println("[ERROR] Coefficient matrix smaller than 1: " + function)
      return ""
    }
    val zIndices = function.unboundGroundedPredicates.map(gp => "x" + gp.id)
    val vector = coefficientMatrix.zipWithIndex.map { case (c, i) => s"${if (c > 0) s" + $c" else s" - ${-c}"} ${zIndices(i)}" }
    s"c${function.id}: ${vector.mkString(" ")} <=  ${constant}"
  }
}

object LpResultParser extends com.signalcollect.psl.parser.ParseHelper[Map[Int, Double]] with ImplicitConversions {

  /**
   * VARIABLES
   * INDEX      NAME           AT ACTIVITY                 LOWER LIMIT        UPPER LIMIT
   * 0          X1             SB 0.00000000000000e+00     0.00000000e+00     1.00000000e+00
   *
   *   DUAL LOWER               DUAL UPPER
   */

  lexical.delimiters ++= List("(", ")", "&", "|", "=>", "=", "!", ",", ":", "[", "]")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = variables

  def parse(files: List[File]): Map[Int, Double] = {
    val parsedFiles = files.map(parseFileLineByLine(_))
    parsedFiles.foldLeft(Map.empty[Int, Double])(_ ++ _)
  }

  override def parse(f: File): Map[Int, Double] = {
    parseFileLineByLine(f)
  }

  def parseFileLineByLine(file: File): Map[Int, Double] = {
    val iterator = io.Source.fromFile(file).getLines
    var isVariableList = false
    val parsedLines = iterator.toList.flatMap {
      line =>
        if (line.contains("VARIABLES")) {
          isVariableList = true
          None
        } else if (!isVariableList || line.contains("INDEX") || line.contains(":") || line.trim == "") {
          None
        } else {
          Some(parseString(line, variable))
        }
    }.flatten.toMap
    parsedLines
  }

  lazy val variable: Parser[Option[(Int, Double)]] = {
    integer ~ identifier ~ identifier ~ double ~ (double | "NONE") ~ (double | "NONE") ~ opt(double) ~ opt(double) ^^ {
      case index ~ varName ~ at ~ activity ~ lowerLimit ~ upperLimit ~ optDualLower ~ optDualUpper =>
        Some((varName.stripPrefix("X").stripPrefix("x").toInt, activity.toDouble))
    }
  }

  lazy val variables: Parser[Map[Int, Double]] = {
    rep(variable) ^^ {
      case variables => variables.flatten.toMap
    }
  }

}
