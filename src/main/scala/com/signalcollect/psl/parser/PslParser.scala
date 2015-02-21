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

  lexical.delimiters ++= List("(", ")", "&", "|", "=>", "=", "!", ",", ":", "[", "]")
  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = pslFile
  def fragmentParser = pslFileFragment
  
  val maxPossibleCardinality = 10
  
  def parse(files: List[File]): ParsedPslFile= {
    val parsedFiles = files.map(parseFileLineByLine(_))
    parsedFiles.foldLeft(ParsedFile()) (_ merge _).toParsedPslFile()
  }
  
  def parseNonParallel(files: List[File]): ParsedPslFile= {
    val parsedFiles = files.map(parseFile(_, fragmentParser))
    parsedFiles.foldLeft(ParsedFile()) (_ merge _).toParsedPslFile()
  }
  
  def parseFileLineByLine(file: File): ParsedFile = {
    val chunkSize = 12800 * 1024
    val iterator = io.Source.fromFile(file).getLines.grouped(chunkSize)
    val parsedLines = iterator.flatMap { lines =>
      lines.par.map { line => parseString(line, fragmentParser) }
    }
    parsedLines.foldLeft(ParsedFile()) (_ merge _)
  }
  
  var ruleId = 0
  
  
  /**
   * Class types and individuals. 
   */
  
  lazy val classType: Parser[(PslClass, Set[Individual])] = {
    "class" ~> (identifier|regexUrl) ~ opt(":" ~ repsep(identifier|regexUrl, ",")) ^^ {
      case classType ~ optionalList =>
        if(!optionalList.isDefined){
          (PslClass(classType), Set.empty)
        } else {
          val individuals = optionalList.get._2
          for (ind <- individuals){
            assert(!ind.charAt(0).isUpper,
            s"Individuals cannot start with an uppercase letter.")
          }
          (PslClass(classType), individuals.map(Individual(_, Set(PslClass(classType)))).toSet)
        }
    }
  }
    
  lazy val individuals: Parser[Set[Individual]] = {
    "individuals" ~> ":" ~ repsep(identifier|regexUrl, ",") ^^ {
      case ":" ~ individuals =>
        for (ind <- individuals){
            assert(ind.forall(c => !c.isUpper),
            s"Individuals that appear in facts have to be all lowercase, ind contains at least one uppercase character.")
        }
        individuals.map(Individual(_)).toSet
    }
  }
  
  /**
   * Predicates and their properties
   */
  
  val validPredicateProperties = Set("prior", "closedWorld")
    
  lazy val predicateProperty: Parser[(String, String)] = {
    opt(identifier <~ "=") ~ "[a-zA-Z0-9\\-\\.]*".r ^^ {
      case propertyNameOpt ~ propertyValue =>
        if (propertyNameOpt.isDefined) {
          val propertyName = propertyNameOpt.get
          assert(validPredicateProperties.contains(propertyName),
            s"$propertyName is not a valid predicate property. Valid predicate properties are:\n" +
              validPredicateProperties.mkString(", "))
          (propertyName, propertyValue)
        } else{
          if (propertyValue.exists(_.isDigit)){
            ("prior", propertyValue)
          } else if (propertyValue.contains("closed")){
            println("[WARNING]: closed world not implemented yet.")
            ("closedWorld", "true")
          } else {
            // Functional, PartialFunctional, etc.
            (propertyValue, propertyValue)
          }
        }
    }
  }
    
  lazy val predicateProperties: Parser[Map[String,String]] = {
    "[" ~> repsep(predicateProperty, ",") <~ "]" ^^ {
      case properties =>
        properties.toMap
    }
  }
  
  lazy val predicate: Parser[Predicate] = {
    "predicate" ~> opt(predicateProperties) ~ ":" ~ (identifier|regexUrl) ~ "(" ~ repsep(nonSetPredicateClass|setPredicateClass, ",") ~ ")" ^^ {
      case properties ~ ":" ~ name ~ "(" ~ placeholders ~")" =>
         val prior = properties.flatMap(_.get("prior")).map(_.toDouble)
         val parsedProperties: Set[PredicateProperty]= properties match{
           case Some(p) => 
             p.filter(_._1 != "prior").map{a => PredicateProperty.parse(a._2)}.toSet
           case None => Set.empty
         }
        Predicate(name, classes = placeholders, properties = parsedProperties, prior = prior)
    }
  }
  lazy val nonSetPredicateClass: Parser[PslClass]= {
    not("Set") ~> (regexUrl|identifierOrDash)^^ {
      case placeholder =>
        PslClass(placeholder)
    }
  }
  
  lazy val setPredicateClass: Parser[PslClass]= {
    "Set" ~ opt("{") ~> opt(integer <~ ",")  ~ opt(integer) ~ opt("}") ~ "[" ~ (identifierOrDash|regexUrl) ~ "]" ^^ {
      case  minCardinality ~ maxCardinality ~ optionalBracket ~ "[" ~ singleClassType ~ "]" =>
        assert(maxCardinality.getOrElse(maxPossibleCardinality) <= maxPossibleCardinality, s"Maximum cardinality is bound by $maxPossibleCardinality")
        assert(minCardinality.getOrElse(0) <= maxCardinality.getOrElse(maxPossibleCardinality), "Minimum cardinality should be less than maximum cardinality")
        PslClass(singleClassType, true, minCardinality, maxCardinality)
    }
  }
  
  /**
   * Rules and their properties.
   */

  val validRuleProperties = Set("weight", "distanceMeasure")

  lazy val ruleProperty: Parser[(String, String)] = {
    opt(identifier <~ "=") ~ "[a-zA-Z0-9\\-\\.]*".r ^^ {
      case propertyNameOpt ~ propertyValue =>
        if (propertyNameOpt.isDefined) {
          val propertyName = propertyNameOpt.get
          assert(validRuleProperties.contains(propertyName),
            s"$propertyName is not a valid rule property. Valid rule properties are:\n" +
              validRuleProperties.mkString(", "))
          (propertyName, propertyValue)
        } else {
          if (propertyValue.exists(_.isDigit)){
            ("weight", propertyValue)
          } else {
            ("distanceMeasure", propertyValue)
          }
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
  
  lazy val predicateInRule: Parser[PredicateInRule] = {
    opt("!") ~ (identifier|regexUrl) ~ "(" ~ repsep(varOrIndividualsInSet|varOrIndividualsNotInSet, ",") <~ ")" ^^ {
      case negation ~ predicateName ~ "(" ~ variableOrIndividualNames =>
        val variablesOrIndividuals = variableOrIndividualNames.map{
          v => 
            VariableOrIndividual(v.toString)
        }
        PredicateInRule(predicateName, variablesOrIndividuals , negation.isDefined)
    }
  }

  lazy val rule: Parser[Rule] = {
    "rule" ~> opt(ruleProperties) ~ ":" ~ opt(existsInSetClause) ~ opt( bodyClauses <~ "=>") ~ opt(foreachInSetClause) ~ headClauses ^^ {
      case properties ~ ":" ~ existsInSetClauseInBody ~ bodyClause ~ foreachInSetClauseInHead ~ headClause =>
        // If the properties map contains a 'distanceMeasure' property,
        val distanceMeasure = properties.flatMap(_.get("distanceMeasure")).
          // parse it and use it, else use the default measure.
          map(DistanceMeasure.parse(_)).getOrElse(DistanceMeasure.defaultMeasure)
        // If the properties map contains a 'weight' property,
        val weight = properties.flatMap(_.get("weight")).
          //  parse it and use that weight, else use weight 'hardRuleWeight' to simulate a hard rule.
          map(_.toDouble).getOrElse(hardRuleWeight)
        ruleId += 1
        val (bodyPredicates, foreachInSetClauseInBody) = bodyClause match {
          case Some(x) => (x._1, x._2)
          case None => (List.empty, Set.empty[(String, String, Int, Int, String)])
        }
        Rule(ruleId, bodyPredicates, headClause._1, distanceMeasure, weight, headClause._2, 
            foreachInSetClauseInHead.getOrElse(List.empty).toSet, headClause._3.toSet, 
            foreachInSetClauseInBody, existsInSetClauseInBody.getOrElse(List.empty).toSet)
    }
  }
  
   lazy val headClauses: Parser[(List[PredicateInRule], Set[String], List[(String,String, Int, Int,String)])] ={
     repsep(headClause, "|" ~ opt("|")) ^^ {
       case clauses =>
        val predicatesInRules = clauses.flatMap(_._1)
        val setOfExistentialVars = clauses.map(_._2).flatten.toSet
        val existsInSetInHeadClauses =  clauses.flatMap(_._3)
        (predicatesInRules, setOfExistentialVars, existsInSetInHeadClauses )
     }
   }
   
  lazy val headClause: Parser[(Option[PredicateInRule], Set[String], List[(String,String, Int, Int, String)])] ={
    opt(existentialClause|existsInSetClause) ~ opt(predicateInRule) ^^ {
      case None ~ p =>
       (p, Set.empty, List.empty)
      case Some(exist) ~ p =>
        exist match{
          case s : Set[String] => (p, s, List.empty)
          case s : List[(String,String,Int, Int, String)] => (p, Set.empty, s)      
        }
    }
  }
  
  lazy val bodyClause: Parser[(Option[PredicateInRule], Set[(String, String, Int, Int, String)])] ={
    opt(foreachInSetClause) ~ opt(predicateInRule) ^^ {
      case None ~ p => (p, Set.empty)
      case Some(s) ~ p => (p, s.toSet)
    }
  }
  
   lazy val bodyClauses: Parser[(List[PredicateInRule], Set[(String,String, Int, Int, String)])] ={
     repsep(bodyClause, "&" ~ opt("&")) ^^ {
       case clauses =>
         val predicatesInRules = clauses.flatMap(_._1)
         val foreachInSetInBodyClauses = clauses.map(_._2).flatten.toSet
        (predicatesInRules, foreachInSetInBodyClauses )
     }
   }
  
  lazy val existentialClause: Parser[Set[String]] ={
    "EXISTS" ~ "[" ~> repsep(identifier, ",") <~ "]" ^^ {
      case  existVars => existVars.toSet
    }
  }
  
  lazy val foreachInSetClause: Parser[List[(String, String, Int, Int, String)]] ={
    ("FORALL"|"FOREACH") ~ "[" ~> repsep(scopedVariableCouple, ",") <~ "]" ^^ {
      case foreachCouples => foreachCouples
    }
  }
  
  lazy val existsInSetClause: Parser[List[(String, String, Int, Int, String)]] ={
    "EXISTS" ~ "[" ~> repsep(scopedVariableCouple, ",") <~ "]" ^^ {
      case existsCouples => existsCouples
    }
  }
  
  lazy val scopedVariableCouple: Parser[(String, String, Int, Int, String)] ={
    identifier ~ opt ("(") ~ opt(integer <~ ",")  ~ opt(integer) ~ opt(")") ~ containmentOperator ~ identifier ^^ {
      case iterator ~ optBracket ~ minCardinality ~ maxCardinality ~ optClosingBracket~ containmentOption ~ iterable =>
        assert(maxCardinality.getOrElse(maxPossibleCardinality) <= maxPossibleCardinality, s"Maximum cardinality is bound by $maxPossibleCardinality")
        assert(minCardinality.getOrElse(0) <= maxCardinality.getOrElse(maxPossibleCardinality), "Minimum cardinality should be less than maximum cardinality")
        (iterator, iterable, minCardinality.getOrElse(0), maxCardinality.getOrElse(maxPossibleCardinality), containmentOption)
    }
  }

  lazy val containmentOperator: Parser[String] = {
    ("in"|"subsetOf"|"strictSubsetOf"|"IN"|"SUBSET"|"STRICTSUBSET"| "SUBSETOF"| "STRICTSUBSETOF"|"subset"|"strictsubset"|"strictSubset"|"strictsubsetof") ^^ {
      case "in"|"IN" => "in"
      case "subsetOf"|"SUBSET"|"SUBSETOF"|"subset" => "subsetOf"
      case _ => "strictSubsetOf"
    }
  }
  
  lazy val truthValues: Parser[List[Double]] = {
    "[" ~> opt("truthValue" ~> "=") ~> repsep(double, ",") <~ "]" ^^ {
      case truthValues => 
        assert(truthValues.size <= 2, "Too many truth values for fact.")
        truthValues.foreach(t => assert(t <= 1 && t >= 0, "Truth values have to be between 0 and 1"))
        truthValues
    }
  }

  lazy val fact: Parser[Fact] = {
    "fact" ~> opt(truthValues) ~ ":" ~ opt("!") ~ (identifier|regexUrl) ~ "(" ~ individualsInFact <~ ")" ^^ {
      case truthValues ~ ":" ~ negation ~ predicateName ~ "(" ~ variableGroundings =>
        val factTruth = 
          if (truthValues.isDefined && truthValues.get.size == 1 ){
          // fact [ 0.1]: votes(anna, demo)
            if (!negation.isDefined) {
              Some(truthValues.get(0))
            } else {
              Some(1.0- truthValues.get(0))
            }
          } else if (!truthValues.isDefined || truthValues.get.size == 0 ){
            // fact : votes(anna, demo)
            if (!negation.isDefined) {
              Some(1.0)
            } else {
              Some(0.0)
            }
          } else {
            // fact [0.1, 0.3]: votes(anna, demo)
            // an interval fact, we will take care of it in normalizedFactTruth1 and normalizedFactTruth2
            None
          }
        val normalizedFactTruth1 = if (!truthValues.isDefined || truthValues.get.size <= 1){
          None
        } else{
          if (!negation.isDefined) {
            Some(truthValues.get(0)) 
          } else {
            Some(1 - truthValues.get(0)) 
          }
        }   
        val normalizedFactTruth2 = if (!truthValues.isDefined || truthValues.get.size <= 1){
          None
        } else{
          if (!negation.isDefined) {
            Some(truthValues.get(1)) 
          } else {
            Some(1 - truthValues.get(1)) 
          }
        }
        Fact(predicateName, variableGroundings, factTruth, minTruthValue = normalizedFactTruth1, maxTruthValue = normalizedFactTruth2)
    }
  }

  lazy val individualsInFact: Parser[List[Set[Individual]]] = {
    repsep(varOrIndividualsInSet|varOrIndividualsNotInSet, ",") ^^ {
      case List(List("")) =>
        List.empty
      case variableGroundings =>
        for (individuals <- variableGroundings) {
          for (ind <- individuals) {
            assert(!ind.charAt(0).isUpper, s"Individuals cannot start with an uppercase letter.")
          }
        }
        variableGroundings.map( i => i.map(Individual(_)))
    }
  }
  
  lazy val varOrIndividualsNotInSet: Parser[Set[String]] = {
    (regexUrl|identifierOrDash) ^^ {
      case indNonInSet =>
        Set(indNonInSet)
    }
  }
  lazy val varOrIndividualsInSet : Parser[Set[String]] = {
    ("{"|"[") ~> repsep(identifier|regexUrl, ",")  <~ ("}"|"]") ^^ {
       case  indInSet =>
         indInSet.toList.sorted.toSet 
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
}