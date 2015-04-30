/*
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

package com.signalcollect.external.parser

import java.io.File
import java.io.FileWriter
import java.io.FileReader
import com.signalcollect.admm.optimizers.OptimizableFunction
import com.signalcollect.psl.Optimizer
import com.signalcollect.psl.parser.ParsedPslFile
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.Grounding
import com.signalcollect.psl.model.Individual
import com.signalcollect.psl.model.VariableOrIndividual
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.Rule
import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.Linear
import com.signalcollect.psl.model.PslClass
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
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

object CausalDiscoveryAspParser {

  val variableClass = PslClass("Variable")
  val variableSetClass = PslClass("Variable", true)
  val indepPredicate = Predicate("indep", classes = List(variableClass, variableClass, variableSetClass), properties = Set.empty)
  val emptySet = VariableOrIndividual("Set()", Set(variableSetClass)) match {
    case i: Individual =>
      i
    case _ => throw new ClassCastException
  }

  def updateParsedPslFile(originalParsedPslFile: ParsedPslFile, independenceFile: File, setDescriptionFile: File): ParsedPslFile = {
    val setsMap = CausalDiscoveryAspSetsParser.parse(setDescriptionFile)
    val facts = CausalDiscoveryAspFactsParser.parse(independenceFile)
    val parsedIndependences = parseFacts(originalParsedPslFile.rules.size, setsMap, facts)
    ParsedPslFile(originalParsedPslFile.explicitlyMentionedIndividualsInClasses, originalParsedPslFile.predicates,
      originalParsedPslFile.rules ++ parsedIndependences, originalParsedPslFile.facts, originalParsedPslFile.constants)
  }

  def parseFacts(startingRuleId: Int, setsMap: Map[String, Set[Individual]], facts: List[(Boolean, String, String, String, Double)]): List[Rule] = {
    var id = startingRuleId
    facts.map { fact =>
      val conditioningSet = if (setsMap.contains(fact._4)) {
        Individual(setsMap(fact._4).toString())
      } else {
        emptySet
      }
      // A rule with only the head and the weight, e.g. rule [5]: indep(x,y,{y,z})
      // Last argument needs to be translated to a set of Individuals and then refactored as a single Individual containing a set.
      val factArguments = List(Individual(fact._2), Individual(fact._3), conditioningSet)
      val headPredicate = PredicateInRule(indepPredicate.name, variableOrIndividual = factArguments, negated = fact._1, Some(indepPredicate))
      // println(headPredicate)
      val rule = Rule({ id += 1; id }, body = List.empty, head = List(headPredicate), distanceMeasure = Linear, weight = fact._5)
      rule
    }
  }
}

/*
* Parses sets encoded as:
* ismember(7,1). ismember(7,2). ismember(7,3). 
* returning a map in the form: setId -> Set( element ids) = 7 -> Set(1,2,3).
*/
object CausalDiscoveryAspSetsParser extends com.signalcollect.psl.parser.ParseHelper[Map[String, Set[Individual]]] with ImplicitConversions {

  lexical.delimiters ++= List("(", ")", "&", "|", "=>", "=", "!", ",", ":", "[", "]")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = setDefinition

  def parse(files: List[File]): Map[String, Set[Individual]] = {
    val parsedFiles = files.map(parseFileLineByLine(_))
    parsedFiles.foldLeft(Map.empty[String, Set[Individual]])(_ ++ _)
  }

  override def parse(f: File): Map[String, Set[Individual]] = {
    parseFileLineByLine(f)
  }

  def parseFileLineByLine(file: File): Map[String, Set[Individual]] = {
    val iterator = io.Source.fromFile(file).getLines
    val parsedLines = iterator.toList.flatMap {
      line =>
        if (line.contains("ismember")) {
          Some(parseString(line, setDefinition))
        } else {
          // Ignore.
          None
        }
    }.flatten.toMap
    parsedLines
  }

  lazy val ismember: Parser[(String, String)] = {
    "ismember(" ~> identifier ~ "," ~ identifier <~ ")" ^^ {
      case set ~ "," ~ member =>
        (set, member)
    }
  }

  lazy val setDefinition: Parser[Map[String, Set[Individual]]] = {
    opt(("cset(" | "jset(") ~ integer ~ ").") ~> repsep(ismember, ".") <~ opt(".") ^^ {
      case setMembers => setMembers.groupBy(_._1).map { case (k, v) => (k, v.map(b => Individual(b._2)).toSet) }.toMap
    }
  }

}

/*
* Parses (in) dependences encoded as:
* dep(X,Y,C,J,M,W)
* returning a tuple (indep?, X,Y,C,W) that can be merged with the output of the sets parser.
*/
object CausalDiscoveryAspFactsParser extends com.signalcollect.psl.parser.ParseHelper[List[(Boolean, String, String, String, Double)]] with ImplicitConversions {

  lexical.delimiters ++= List("(", ")", "&", "|", "=>", "=", "!", ",", ":", "[", "]")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = fact

  def parse(files: List[File]): List[(Boolean, String, String, String, Double)] = {
    val parsedFiles = files.map(parseFileLineByLine(_))
    parsedFiles.foldLeft(List.empty[(Boolean, String, String, String, Double)])(_ ++ _)
  }

  override def parse(f: File): List[(Boolean, String, String, String, Double)] = {
    parseFileLineByLine(f)
  }

  def parseFileLineByLine(file: File): List[(Boolean, String, String, String, Double)] = {
    io.Source.fromFile(file).getLines.toList.flatMap {
      line =>
        val partsOfLine = line.split("%")
        if (partsOfLine.size == 2 && partsOfLine(0).size > 16) {
          parseString(partsOfLine(0), fact)
        } else {
          List.empty
        }
    }
  }

  lazy val fact: Parser[List[(Boolean, String, String, String, Double)]] = {
    ("dep" | "indep") ~ "(" ~ identifier ~ "," ~ identifier ~ "," ~ identifier ~ "," ~ identifier ~ "," ~ identifier ~ "," ~ double ~ ")" ~ "." ^^ {
      case "indep" ~ "(" ~ x ~ "," ~ y ~ "," ~ c ~ "," ~ j ~ "," ~ m ~ "," ~ w ~ ")" ~ "." =>
        List((false, x, y, c, w / 1000))
      case "dep" ~ "(" ~ x ~ "," ~ y ~ "," ~ c ~ "," ~ j ~ "," ~ m ~ "," ~ w ~ ")" ~ "." =>
        List((true, x, y, c, w / 1000))
    }
  }

}
