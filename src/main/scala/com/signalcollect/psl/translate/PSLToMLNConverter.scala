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
import java.io.FileWriter
import java.io.FileReader
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.psl.Optimizer
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedConstraint
import com.signalcollect.psl.model.GroundedRule
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

object PSLToMLNConverter {
  def toMLN(pslString: String): (String, String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslString)
    toMLN(pslData)
  }

  def toMLN(pslFile: File): (String, String, Map[Int, GroundedPredicate]) = {
    val pslData = PslParser.parse(pslFile)
    println(s"File ${pslFile.getName()} parsed.")
    toMLN(pslData)
  }

  def toMLN(pslData: ParsedPslFile): (String, String, Map[Int, GroundedPredicate]) = {
    val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(pslData)
    println(s"Grounding completed: ${groundedRules.size} grounded rules, ${groundedConstraints.size} constraints and ${idToGpMap.keys.size} grounded predicates.")

    // Write all possible individuals as facts of the predicate Variable(v) in evidence.db
    val evidence = toIndividuals(groundedRules, groundedConstraints)

    // Write all necessary predicates, all with the class individual as arguments.
    val classes = "\nIndividual(individual)"
    val predicates = pslData.predicates.map {
      p =>
        val arguments = p.classes.map(_ => "individual").mkString("(", ", ", ")")
        s"${p.name}${arguments}"
    }.mkString("\n", "\n", "\n")

    // Write all possible grounded rules in CNF form.
    val rules = toRulesInCNF(groundedRules, groundedConstraints)

    (evidence, classes + predicates + rules, idToGpMap)
  }

  def getIndividuals(rules: List[GroundedRule], constraints: List[GroundedConstraint]) = {
    (rules ++ constraints).flatMap(rule => rule.allGroundedPredicates.flatMap(_.groundings).toSet).toSet
  }

  def toIndividuals(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    val individuals = getIndividuals(rules, constraints)
    getIndividuals(rules, constraints).map(i => s"""Individual(\"$i\")""").mkString("\n")
  }

  def toRulesInCNF(rules: List[GroundedRule], constraints: List[GroundedConstraint]): String = {
    val maxWeight = rules.filter(_.definition.weight != Double.MaxValue).maxBy(_.definition.weight).definition.weight
    rules.map(toMLNRule(_, maxWeight)).flatten.mkString("\n") + "\n" + constraints.map(toMLNConstraint(_)).flatten.mkString("\n")
  }

  def toMLNRule(rule: GroundedRule, maxWeight: Double) = {
    // Example: 0.25 !disjoint2(a,b) v !subsumes1(c,d) v !map(a,c) v !map(b,d)
    // but with grounded rules: !disjoint("a","b").
    // Not body1 or not body2 .. or head.
    var ignoreRule = false
    val bodyContribution = rule.body.zipWithIndex.flatMap {
      case (b, i) =>
        if (b.truthValue.isDefined) {
          val bodyTruthValue = b.truthValue.get
          val truthValue = if (bodyTruthValue != 1.0 && bodyTruthValue != 0.0) {
            println("[Warning]: cannot have soft truth values in MLN translation, rounded to closest of 0,1. ")
            if (bodyTruthValue > 0.5) 1.0 else 0.0
          } else { bodyTruthValue }

          if (rule.definition.body(i).negated) {
            if (truthValue == 1) {
              // If a negated body element is 1, then the rule is trivially satisfied.
              ignoreRule = true
              None
            }
            // If a negated body element is 0, we can ignore it.
            None
          } else {
            if (truthValue == 0) {
              // If a nonnegated body element is 0, then the rule is trivially satisfied.
              ignoreRule = true
            }
            // If a nonnegated body element is 1, we can ignore it
            None
          }
        } else {
          Some(b.groundings.map(g => s"""\"$g\"""").mkString(s"${
            if (rule.definition.body(i).negated) { "!" } else { "" }
          }${b.definition.name} (", ",", ")"))
        }
    }

    val headContribution = rule.head.zipWithIndex.flatMap {
      case (b, i) =>
        if (b.truthValue.isDefined) {
          val bodyTruthValue = b.truthValue.get
          val truthValue = if (bodyTruthValue != 1.0 && bodyTruthValue != 0.0) {
            println("[Warning]: cannot have soft truth values in MLN translation, rounded to closest of 0,1. ")
            if (bodyTruthValue > 0.5) 1.0 else 0.0
          } else { bodyTruthValue }
          if (rule.definition.head(i).negated) {
            if (truthValue == 0) {
              // If a negated head element is 0, then the rule is trivially satisfied.
              ignoreRule = true
            }
            // If a negated head element is 1, we can ignore it.
            None
          } else {
            if (truthValue == 1) {
              // If a nonnegated head element is 1, then the rule is trivially satisfied.
              ignoreRule = true
            }
            // If a nonnegated head element is 0, we can ignore it
            None

          }
        } else {
          Some(b.groundings.map(g => s"""\"$g\"""").mkString(s"${
            if (rule.definition.head(i).negated) { "!" } else { "" }
          }${b.definition.name} (", ",", ")"))
        }
    }

    if (!ignoreRule) {
      val totalRule = (bodyContribution ++ headContribution).mkString(" v ")
      if (rule.definition.weight != Double.MaxValue) {
        Some(s"${rule.definition.weight / maxWeight} $totalRule")
      } else {
        Some(s"$totalRule .")
      }
    } else {
      None
    }

  }
  def toMLNConstraint(constraint: GroundedConstraint) = {
    // Example: !disjoint2(a,b) v !subsumes1(c,d) v !map(a,c) v !map(b,d).
    println("[Warning]: Constraint are not translated yet to MLN.")
    None
  }

}
