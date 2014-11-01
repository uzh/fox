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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.signalcollect.psl.model._
import com.signalcollect.util.TestAnnouncements

class PslParserSpec extends FlatSpec with Matchers with TestAnnouncements {

  "PslParser" should "correctly parse a small PSL file" in {
    val parsed = PslParser.parse("""
	predicate: votes(_, _)
	predicate: idol(_, _)
	
	rule [weight = 0.5, distanceMeasure = linear]: 	votes(A,P) && idol(B,A)  => votes(B,P)  || votes(B, A)
	rule [weight = 0.3]: votes(A,P) && !idol(B,A) => !votes(B,P) || votes(B, A) // default distance function (no squared)
	rule: votes(A,P) && idol(B,A)  => votes(B, A) // default weight and distance function
	
	fact: votes(anna, republicans)
	fact [truthValue = 0.5]: votes(anna, democrats)

  predicate [Functional, Symmetric]: samePerson(_, _)
  predicate [PartialFunctional, Symmetric]: hasSpouse(_, _)
  predicate [InverseFunctional]: fatherOf(_, _)
  predicate [InversePartialFunctional]: hasCar(_, _) // assumption: each car has at most one owner
	""")

    // Test individuals.
    assert(parsed.individuals.size == 3)
    assert(parsed.individuals(0).name == "anna")
    assert(parsed.individuals(1).name == "republicans")
    assert(parsed.individuals(2).name == "democrats")

    // Test predicate declarations.
    val votes = parsed.predicates(0)
    assert(votes.name === "votes")
    assert(votes.arity === 2)
    val samePerson = parsed.predicates(2)
    assert(samePerson.name === "samePerson")
    assert(samePerson.arity === 2)
    assert(samePerson.properties === Set(Functional, Symmetric))
    val hasSpouse = parsed.predicates(3)
    assert(hasSpouse.name === "hasSpouse")
    assert(hasSpouse.arity === 2)
    assert(hasSpouse.properties === Set(PartialFunctional, Symmetric))
    val fatherOf = parsed.predicates(4)
    assert(fatherOf.name === "fatherOf")
    assert(fatherOf.arity === 2)
    assert(fatherOf.properties === Set(InverseFunctional))

    // Test rule declarations.
    val r1 = parsed.rules(0)
    assert(r1.weight === 0.5)
    assert(r1.distanceMeasure === Linear)
    assert(r1.body === List(PredicateInRule("votes", List(Variable("A"), Variable("P"))), PredicateInRule("idol", List(Variable("B"), Variable("A")))))
    assert(r1.head === List(PredicateInRule("votes", List(Variable("B"), Variable("P"))), PredicateInRule("votes", List(Variable("B"), Variable("A")))))

    val r2 = parsed.rules(1)
    assert(r2.weight === 0.3)
    assert(r2.distanceMeasure === DistanceMeasure.defaultMeasure)
    assert(r2.body === List(PredicateInRule("votes", List(Variable("A"), Variable("P"))), PredicateInRule("idol", List(Variable("B"), Variable("A")), negated = true)))
    assert(r2.head === List(PredicateInRule("votes", List(Variable("B"), Variable("P")), negated = true), PredicateInRule("votes", List(Variable("B"), Variable("A")))))

    val r3 = parsed.rules(2)
    assert(r3.weight === PslParser.hardRuleWeight)
    assert(r3.distanceMeasure === DistanceMeasure.defaultMeasure)
    assert(r3.body === List(PredicateInRule("votes", List(Variable("A"), Variable("P"))), PredicateInRule("idol", List(Variable("B"), Variable("A")))))
    assert(r3.head === List(PredicateInRule("votes", List(Variable("B"), Variable("A")))))

    //Test for facts.
    parsed.facts.size should equal(2)
    parsed.facts(0).name should equal("votes")
    parsed.facts(0).truthValue should equal (Some(1.0))
    parsed.facts(0).variableGroundings(0).name should equal("anna")
    parsed.facts(0).variableGroundings(1).name should equal("republicans")

    parsed.facts(1).name should equal("votes")
    parsed.facts(1).truthValue should equal(Some(0.5))
    parsed.facts(1).variableGroundings(0).name should equal("anna")
    parsed.facts(1).variableGroundings(1).name should equal("democrats")

  }

  it should "correctly parse a small PSL file without facts" in {
    val parsed = PslParser.parse("""
	predicate: votes(_, _)
	predicate [Functional, Symmetric]: 				samePerson(_, _)
	predicate [PartialFunctional, Symmetric]: 		hasSpouse(_, _)
	predicate [InverseFunctional]: 					fatherOf(_, _)
	predicate [InversePartialFunctional]: 			hasCar(_, _) // assumption: each car has at most one owner
  predicate: idol(_, _)
	
    rule [weight = 0.5, distanceMeasure = linear]: 	votes(A,P) && idol(B,A)  => votes(B,P)  || votes(B, A)
	""")

    // Test predicate declarations.
    val votes = parsed.predicates(0)
    assert(votes.name === "votes")
    assert(votes.arity === 2)
    val samePerson = parsed.predicates(1)
    assert(samePerson.name === "samePerson")
    assert(samePerson.arity === 2)
    assert(samePerson.properties === Set(Functional, Symmetric))
    val hasSpouse = parsed.predicates(2)
    assert(hasSpouse.name === "hasSpouse")
    assert(hasSpouse.arity === 2)
    assert(hasSpouse.properties === Set(PartialFunctional, Symmetric))
    val fatherOf = parsed.predicates(3)
    assert(fatherOf.name === "fatherOf")
    assert(fatherOf.arity === 2)
    assert(fatherOf.properties === Set(InverseFunctional))

  }

  it should "correctly parse a small PSL file without rules" in {
    val parsed = PslParser.parse("""
	predicate: 										votes(_, _)
	predicate [Functional, Symmetric]: 				samePerson(_, _)
	predicate [PartialFunctional, Symmetric]: 		hasSpouse(_, _)
	predicate [InverseFunctional]: 					fatherOf(_, _)
	predicate [InversePartialFunctional]: 			hasCar(_, _) // assumption: each car has at most one owner
	
	fact: votes(anna, republicans)
	fact [truthValue = 0.5]: votes(anna, democrats)
	""")

    // Test individuals.
    assert(parsed.individuals.size == 3)
    assert(parsed.individuals(0).name == "anna")
    assert(parsed.individuals(1).name == "republicans")
    assert(parsed.individuals(2).name == "democrats")

    // Test predicate declarations.
    val votes = parsed.predicates(0)
    assert(votes.name === "votes")
    assert(votes.arity === 2)
    val samePerson = parsed.predicates(1)
    assert(samePerson.name === "samePerson")
    assert(samePerson.arity === 2)
    assert(samePerson.properties === Set(Functional, Symmetric))
    val hasSpouse = parsed.predicates(2)
    assert(hasSpouse.name === "hasSpouse")
    assert(hasSpouse.arity === 2)
    assert(hasSpouse.properties === Set(PartialFunctional, Symmetric))
    val fatherOf = parsed.predicates(3)
    assert(fatherOf.name === "fatherOf")
    assert(fatherOf.arity === 2)
    assert(fatherOf.properties === Set(InverseFunctional))

    //Test for facts.
    parsed.facts.size should equal(2)
    parsed.facts(0).name should equal("votes")
    parsed.facts(0).truthValue should equal (Some(1.0))
    parsed.facts(0).variableGroundings(0).name should equal("anna")
    parsed.facts(0).variableGroundings(1).name should equal("republicans")

    parsed.facts(1).name should equal("votes")
    parsed.facts(1).truthValue should equal(Some(0.5))
    parsed.facts(1).variableGroundings(0).name should equal("anna")
    parsed.facts(1).variableGroundings(1).name should equal("democrats")

  }

  it should "support rules with syntactic sugar for the weight" in {
    val parsed = PslParser.parse("""
  predicate: votes(_, _)
  predicate: idol(_, _)
	rule [0.5, distanceMeasure = linear]: 	votes(A,P) && idol(B,A)  => votes(B,P)  || votes(B, A)
	rule [0.3]: 							votes(A,P) && !idol(B,A) => !votes(B,P) || votes(B, A) // default distance function (no squared)
	""")
    // Test rule declarations.
    val r1 = parsed.rules(0)
    assert(r1.weight === 0.5)
    assert(r1.distanceMeasure === Linear)
    assert(r1.body === List(PredicateInRule("votes", List(Variable("A"), Variable("P"))), PredicateInRule("idol", List(Variable("B"), Variable("A")))))
    assert(r1.head === List(PredicateInRule("votes", List(Variable("B"), Variable("P"))), PredicateInRule("votes", List(Variable("B"), Variable("A")))))

    val r2 = parsed.rules(1)
    assert(r2.weight === 0.3)
    assert(r2.distanceMeasure === DistanceMeasure.defaultMeasure)
    assert(r2.body === List(PredicateInRule("votes", List(Variable("A"), Variable("P"))), PredicateInRule("idol", List(Variable("B"), Variable("A")), negated = true)))
    assert(r2.head === List(PredicateInRule("votes", List(Variable("B"), Variable("P")), negated = true), PredicateInRule("votes", List(Variable("B"), Variable("A")))))
  }

  it should "support facts with syntactic sugar for the weight" in {
    val parsed = PslParser.parse("""
  predicate: votes(_, _)
	fact [0.5]: votes(anna, democrats)
	""")
    //Test for facts.
    parsed.facts.size should equal(1)
    parsed.facts(0).name should equal("votes")
    parsed.facts(0).truthValue should equal(Some(0.5))
    parsed.facts(0).variableGroundings(0).name should equal("anna")
    parsed.facts(0).variableGroundings(1).name should equal("democrats")
  }
  
}
