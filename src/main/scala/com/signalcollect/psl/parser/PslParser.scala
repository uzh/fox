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
import com.signalcollect.psl.model.PslClass
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

  lexical.delimiters ++= List("(", ")", "&", "|", "=>", "=", "!", ",", ":", "_")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = pslFile
  def fragmentParser = pslFileFragment
  
  def parse(files: List[File]): ParsedPslFile= {
    val parsedFiles = files.map(parseFile(_, fragmentParser))
    parsedFiles.foldLeft(ParsedFile()) (_ merge _).toParsedPslFile()
  }
  
  var ruleId = 0

  lazy val predicateInRule: Parser[PredicateInRule] = {
    opt("!") ~ identifier ~ "(" ~ repsep(varOrIndividualsInSet|varOrIndividualsNotInSet, ",") <~ ")" ^^ {
      case negation ~ predicateName ~ "(" ~ variablesOrIndividuals =>
        PredicateInRule(predicateName, variablesOrIndividuals.map(v => VariableOrIndividual(v.toString)), negation.isDefined)
    }
  }

  val validRuleProperties = Set("weight", "distanceMeasure")
  val validPredicateProperties = Set("prior")

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
  
  val predicateProperty: Parser[(String, String)] = {
    opt(identifier <~ "=") ~ "[a-zA-Z0-9\\-\\.]*".r ^^ {
      case propertyNameOpt ~ propertyValue =>
        if (propertyNameOpt.isDefined) {
          val propertyName = propertyNameOpt.get
          assert(validPredicateProperties.contains(propertyName),
            s"$propertyName is not a valid predicate property. Valid predicate properties are:\n" +
              validPredicateProperties.mkString(", "))
          (propertyName, propertyValue)
        } else{
          (propertyValue, propertyValue)
        }
    }
  }
    
  lazy val predicateProperties: Parser[Map[String,String]] = {
    "[" ~> repsep(predicateProperty, ",") <~ "]" ^^ {
      case properties =>
        properties.toMap
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
    "rule" ~> opt(ruleProperties) ~ ":" ~ opt(repsep(predicateInRule, "&" ~ opt("&")) ~ "=>") ~ opt(existentialClause) ~ repsep(predicateInRule, "|" ~ opt("|")) ^^ {
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

  lazy val predicate: Parser[Predicate] = {
    "predicate" ~> opt(predicateProperties) ~ ":" ~ identifier ~ "(" ~ predicateClasses <~ ")" ^^ {
      case properties ~ ":" ~ name ~ "(" ~ placeholders =>
         val prior = properties.flatMap(_.get("prior")).map(_.toDouble)
         val parsedProperties: Set[PredicateProperty]= properties match{
           case Some(p) => 
             p.filter(_._1 != "prior").map{a => PredicateProperty.parse(a._2)}.toSet
           case None => Set.empty
         }
        Predicate(name, classes = placeholders.map(PslClass(_)), properties = parsedProperties, prior = prior)
    }
  }

  lazy val predicateClasses: Parser[List[String]]= {
    repsep(identifierOrDashOrSquareBracket, ",") ^^ {
      case List("") =>
        List.empty
      case placeholders =>
        placeholders
    }
  }
   
  lazy val truthValue: Parser[Double] = {
    "[" ~> opt("truthValue" ~> "=") ~> double <~ "]" ^^ {
      case truthValue => truthValue
    }
  }

  lazy val fact: Parser[Fact] = {
    "fact" ~> opt(truthValue) ~ ":" ~ opt("!") ~ identifier ~ "(" ~ individualsInFact <~ ")" ^^ {
      case truthValue ~ ":" ~ negation ~ predicateName ~ "(" ~ variableGroundings =>
        val factTruth = if (!negation.isDefined) { truthValue.getOrElse(1.0) } else 1 - truthValue.getOrElse(1.0)
        Fact(predicateName, variableGroundings, Some(factTruth))
    }
  }

  lazy val individualsInFact: Parser[List[Set[Individual]]] = {
    repsep(varOrIndividualsInSet|varOrIndividualsNotInSet, ",") ^^ {
      case List(List("")) =>
        List.empty
      case variableGroundings =>
        for (individuals <- variableGroundings) {
          for (individual <- individuals) {
            assert(individual.forall(c => !c.isUpper),
              s"Individuals that appear in facts have to be all lowercase, $individual contains at least one uppercase character.")
          }
        }
        variableGroundings.map( i => i.map(Individual(_)))
    }
  }
  
  lazy val varOrIndividualsNotInSet: Parser[Set[String]] = {
    identifierOrDash ^^ {
      case indNonInSet =>
        Set(indNonInSet)
    }
  }
  lazy val varOrIndividualsInSet : Parser[Set[String]] = {
    ("{"|"[") ~> repsep(identifier, ",")  <~ ("}"|"]") ^^ {
       case indInSet =>
         indInSet.toSet
    } 
  }
    
  lazy val pslFile: Parser[ParsedPslFile] = {
    rep(pslLine) ^^ {
      case lines =>
       val predicates = lines.flatMap{ case f: Predicate => Some(f) case _ => None} 
       val rules = lines.flatMap{ case f: Rule => Some(f) case _ => None}   
       val facts = lines.flatMap{ case f: Fact => Some(f) case _ => None}
       val inds = lines.flatMap{ case f: Set[Individual] => Some(f) case _ => None}
       val classes = lines.flatMap{ case f: (PslClass, Set[Individual]) => Some(f) case _ => None}
       val classMap = classes.groupBy(_._1).mapValues(c => c.map(i => i._2).foldLeft(Set.empty[Individual])(_ ++ _))
       
       if (inds.length == 0 ){
         ParsedPslFile(classMap, predicates, rules, facts)
       }
       else {
         val unionOfIndividuals = inds.foldLeft(inds(0))(_.union(_))
          ParsedPslFile(classMap, predicates, rules, facts, unionOfIndividuals)  
       } 
    }
  }
  
    lazy val pslFileFragment: Parser[ParsedFile] = {
    rep(pslLine) ^^ {
      case lines =>
       val predicates = lines.flatMap{ case f: Predicate => Some(f) case _ => None} 
       val rules = lines.flatMap{ case f: Rule => Some(f) case _ => None}   
       val facts = lines.flatMap{ case f: Fact => Some(f) case _ => None}
       val inds = lines.flatMap{ case f: Set[Individual] => Some(f) case _ => None}
       val classes = lines.flatMap{ case f: (PslClass, Set[Individual]) => Some(f) case _ => None}
       val classMap = classes.groupBy(_._1).mapValues(c => c.map(i => i._2).foldLeft(Set.empty[Individual])(_ ++ _))
       if (inds.length == 0 ){
         ParsedFile(classMap, predicates, rules, facts)
       }
       else {
         val unionOfIndividuals = inds.foldLeft(inds(0))(_.union(_))
          ParsedFile(classMap, predicates, rules, facts, unionOfIndividuals)  
       } 
    }
  }

 lazy val pslLine: Parser[Any] = {
    (classType|predicate|rule|fact|individuals) ^^ {
      case line =>
        line
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
  
  lazy val classType: Parser[(PslClass, Set[Individual])] = {
    "class" ~> identifier ~ opt(":") ~ repsep(identifier, ",") ^^ {
      case classType ~ colon ~ List("")  =>
        (PslClass(classType), Set.empty)
      case classType ~ colon ~ individuals =>
        for (ind <- individuals){
            assert(!ind.charAt(0).isUpper,
            s"Individuals that appear in facts must start with a lowercase character.")
        }
        (PslClass(classType), individuals.map(Individual(_, Set(PslClass(classType)))).toSet)
    }
  }
}