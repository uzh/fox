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

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader
import com.signalcollect.psl.model.DistanceMeasure
import com.signalcollect.psl.model.Individual
import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.PredicateProperty
import com.signalcollect.psl.model.Rule
import com.signalcollect.psl.model.Variable
import java.io.File
import scala.util.parsing.input.StreamReader
import java.io.BufferedReader
import java.io.FileReader
import scala.util.parsing.input.Reader
import scala.annotation.tailrec
import com.signalcollect.psl.model.VariableOrIndividual

/**
 * A parser of text files in our own format:
 *
 * predicate: votes(_, _)
 * predicate [functional, symmetric]: samePerson(_, _)
 * predicate [partialFunctional, symmetric]: hasSpouse(_, _)
 * predicate [inverseFunctional]: fatherOf(_, _)
 * predicate [inversePartialFunctional]: hasCar(_, _) // assumption: each car has at most one owner
 *
 * rule [weight = 0.5, distanceMeasure = linear]: votes(A,P) && idol(B,A)  => votes(B,P) || votes(B, A)
 * rule [weight = 0.3]: votes(A,P) && idol(B,A) => votes(B,P) || votes(B, A) // default distance function (no squared)
 * rule: votes(A,P) && idol(B,A) => votes(B,P) || votes(B, A) // default weight and distance function
 *
 * fact: votes(anna, republicans) // default truth value = 1.0
 * fact: !votes(anna, democrats) // default truth value = 1.0
 * fact [truthValue = 0.5]: votes(anna, republicans)
 */
object PslParser extends ParseHelper[ParsedPslFile] with ImplicitConversions {

  lexical.delimiters ++= List(
    "(", ")", "&&", "||", "=>", "=", "!", ",", ":", "_")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = pslFile
  
  var ruleId = 0

  lazy val predicateInRule: Parser[PredicateInRule] = {
    opt("!") ~ identifier ~ "(" ~ repsep(identifier, ",") <~ ")" ^^ {
      case negation ~ predicateName ~ "(" ~ variablesOrIndividuals =>
        PredicateInRule(predicateName, variablesOrIndividuals.map(VariableOrIndividual(_)), negation.isDefined)
    }
  }

  val validRuleProperties = Set("weight", "distanceMeasure")

  val ruleProperty: Parser[(String, String)] = {
    opt(identifier <~ "=") ~ "[a-zA-Z0-9\\-\\.]*".r ^^ {
      case propertyNameOpt ~ propertyValue =>
        if (propertyNameOpt.isDefined) {
          val propertyName = propertyNameOpt.get
          assert(validRuleProperties.contains(propertyName),
            s"$propertyName is not a valid rule property. Valid rule properties are:\n" +
              validRuleProperties.mkString(", "))
          (propertyName, propertyValue)
        } else {
          ("weight", propertyValue)
        }
    }
  }

  lazy val ruleProperties: Parser[Map[String, String]] = {
    "[" ~> repsep(ruleProperty, ",") <~ "]" ^^ {
      case ruleProperties =>
        ruleProperties.toMap
    }
  }

  // The hard rule weight is ~ infinite. 
  // It doesn't matter for overflow, since we use linear constraints for their implementation instead of standard hingeloss.
  val hardRuleWeight = Double.MaxValue

  lazy val rule: Parser[Rule] = {
    "rule" ~> opt(ruleProperties) ~ ":" ~ opt(repsep(predicateInRule, "&&") ~ "=>") ~ opt(existentialClause) ~ repsep(predicateInRule, "||") ^^ {
      case properties ~ ":" ~ bodyClause ~ existClause ~ headClause =>
        // If the properties map contains a 'distanceMeasure' property,
        val distanceMeasure = properties.flatMap(_.get("distanceMeasure")).
          // parse it and use it, else use the default measure.
          map(DistanceMeasure.parse(_)).getOrElse(DistanceMeasure.defaultMeasure)
        // If the properties map contains a 'weight' property,
        val weight = properties.flatMap(_.get("weight")).
          //  parse it and use that weight, else use weight 'hardRuleWeight' to simulate a hard rule.
          map(_.toDouble).getOrElse(hardRuleWeight)
        ruleId += 1
        val bodyPredicates = bodyClause match {
          case Some(x) => x._1 
          case None => List.empty
        }
        Rule(ruleId, bodyPredicates, headClause, distanceMeasure, weight, existClause.getOrElse(Set.empty))
    }
  }
   
  lazy val existentialClause: Parser[Set[String]] ={
    "EXISTS" ~ "[" ~> repsep(identifier, ",") <~ "]" ^^ {
      case existVars => existVars.toSet
    }
  }

  lazy val predicateProperties: Parser[Set[PredicateProperty]] = {
    "[" ~> repsep(identifier, ",") <~ "]" ^^ {
      case properties =>
        properties.map(PredicateProperty.parse(_)).toSet
    }
  }

  lazy val predicate: Parser[Predicate] = {
    "predicate" ~> opt(predicateProperties) ~ ":" ~ identifier ~ "(" ~ repsep(identifierOrDash, ",") <~ ")" ^^ {
      case properties ~ ":" ~ name ~ "(" ~ List("") =>
        Predicate(name, properties = properties.getOrElse(Set.empty))
      case properties ~ ":" ~ name ~ "(" ~ placeholders =>
        Predicate(name, placeholders, properties.getOrElse(Set.empty))
    }
  }

  lazy val truthValue: Parser[Double] = {
    "[" ~> opt("truthValue" ~> "=") ~> double <~ "]" ^^ {
      case truthValue => truthValue
    }
  }

  lazy val fact: Parser[Fact] = {
    "fact" ~> opt(truthValue) ~ ":" ~ opt("!") ~ identifier ~ "(" ~ repsep(identifier, ",") <~ ")" ^^ {
      case truthValue ~ ":" ~ negation ~ predicateName ~ "(" ~ List("") =>
        val factTruth = if (!negation.isDefined) { truthValue.getOrElse(1.0) } else 1 - truthValue.getOrElse(1.0)
        Fact(predicateName, List.empty, Some(factTruth))
      case truthValue ~ ":" ~ negation ~ predicateName ~ "(" ~ variableGroundings =>
        for (individual <- variableGroundings) {
          assert(individual.forall(c => !c.isUpper),
            s"Individuals that appear in facts have to be all lowercase, $individual contains at least one uppercase character.")
        }
        val factTruth = if (!negation.isDefined) { truthValue.getOrElse(1.0) } else 1 - truthValue.getOrElse(1.0)
        Fact(predicateName, variableGroundings.map(Individual(_)), Some(factTruth))
    }
  }

  lazy val pslFile: Parser[ParsedPslFile] = {
    rep(classType) ~! rep(predicate) ~! rep(rule) ~! rep(fact) ~! opt(individuals) ^^ {
      case classes ~ predicates ~ rules ~ groundings ~ individuals =>
        individuals match {
          case Some(ind) =>
          	ParsedPslFile(classes.toMap, predicates, rules, groundings, ind)
          case None => 
            ParsedPslFile(classes.toMap, predicates, rules, groundings)
        }

    }
  }
  
  lazy val individuals: Parser[Set[Individual]] = {
    "individuals" ~> ":" ~ repsep(identifier, ",") ^^ {
      case ":" ~ individuals =>
        for (ind <- individuals){
            assert(ind.forall(c => !c.isUpper),
            s"Individuals that appear in facts have to be all lowercase, ind contains at least one uppercase character.")
        }
        individuals.map(Individual(_)).toSet
    }
  }
  
  lazy val classType: Parser[(String, Set[Individual])] = {
    "class" ~> identifier ~ opt(":") ~ repsep(identifier, ",") ^^ {
      case classType ~ colon ~ List("")  =>
        (classType, Set.empty)
      case classType ~ colon ~ individuals =>
        for (ind <- individuals){
            assert(ind.forall(c => !c.isUpper),
            s"Individuals that appear in facts have to be all lowercase, and $ind contains at least one uppercase character.")
        }
        (classType, individuals.map(Individual(_, Set(classType))).toSet)
    }
  }
}
