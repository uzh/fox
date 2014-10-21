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

package com.signalcollect.psl

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.signalcollect.TestAnnouncements
import com.signalcollect.psl.model.Functional
import com.signalcollect.psl.model.Individual
import com.signalcollect.psl.model.Symmetric
import com.signalcollect.psl.model.Variable
import com.signalcollect.psl.parser.PslParser

class GroundingSpec extends FlatSpec with Matchers with TestAnnouncements {

  val anna = Individual("anna")
  val bob = Individual("bob")
  val demo = Individual("democrats")
  val repub = Individual("republicans")

  "Grounding" should " correctly ground the predicates for a small example" in {
    val pslData = PslParser.parse(""" 
        predicate[Functional]: 	votes(_,_)
        predicate: 	person(_)
        predicate: 	mentor(_, _)
        predicate: 	idol( _, _)
        rule [weight = 0.5, distanceMeasure = linear]: 	person(A) && votes(A,P) && idol(B,A)  => !votes(B,P) 
        fact: !votes(anna, republicans)
        fact: person(anna)
        fact [truthValue = 0.5]: votes(anna, democrats)
        fact [truthValue = 0.666]: votes(bob, republicans)
	""")
    // anna, democrats, republicans, bob
    // Result: Map[(String, List[String]), GroundedPredicate]
    val groundedPredicates = Grounding.createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts, pslData.individualsByClass)
    assert(groundedPredicates.values.size == 28)

    assert(groundedPredicates("person", List(anna)).truthValue == Some(1.0))
    assert(groundedPredicates("person", List(anna)).definition.arity == 1)
    assert(groundedPredicates("person", List(anna)).definition.name == "person")

    assert(groundedPredicates("person", List(bob)).truthValue == None)
    assert(groundedPredicates("person", List(demo)).truthValue == None)
    assert(groundedPredicates("person", List(repub)).truthValue == None)

    assert(groundedPredicates("votes", List(anna, bob)).truthValue == None)
    assert(groundedPredicates("votes", List(anna, demo)).truthValue == Some(0.5))
    assert(groundedPredicates("votes", List(anna, repub)).truthValue == Some(0.0))

    assert(groundedPredicates("votes", List(bob, anna)).truthValue == None)
    assert(groundedPredicates("votes", List(bob, demo)).truthValue == None)
    assert(groundedPredicates("votes", List(bob, repub)).truthValue == Some(0.666))

    assert(groundedPredicates("votes", List(demo, anna)).truthValue == None)
    assert(groundedPredicates("votes", List(demo, bob)).truthValue == None)
    assert(groundedPredicates("votes", List(demo, repub)).truthValue == None)

    assert(groundedPredicates("votes", List(repub, anna)).truthValue == None)
    assert(groundedPredicates("votes", List(repub, bob)).truthValue == None)
    assert(groundedPredicates("votes", List(repub, demo)).truthValue == None)

    assert(groundedPredicates("idol", List(anna, bob)).truthValue == None)
    assert(groundedPredicates("idol", List(anna, demo)).truthValue == None)
    assert(groundedPredicates("idol", List(anna, repub)).truthValue == None)

    assert(groundedPredicates("idol", List(bob, anna)).truthValue == None)
    assert(groundedPredicates("idol", List(bob, demo)).truthValue == None)
    assert(groundedPredicates("idol", List(bob, repub)).truthValue == None)

    assert(groundedPredicates("idol", List(demo, anna)).truthValue == None)
    assert(groundedPredicates("idol", List(demo, bob)).truthValue == None)
    assert(groundedPredicates("idol", List(demo, repub)).truthValue == None)

    assert(groundedPredicates("idol", List(repub, anna)).truthValue == None)
    assert(groundedPredicates("idol", List(repub, bob)).truthValue == None)
    assert(groundedPredicates("idol", List(repub, demo)).truthValue == None)
  }

  "Grounding" should " correctly ground the rules for a small example" in {
    val pslData = PslParser.parse(""" 
        predicate[Functional]: 	votes(_,_)
        predicate[Symmetric]: 	friend( _, _)
        rule [weight = 0.5, distanceMeasure = linear]: 	votes(A,P) && friend(A, B)  => votes(B,P) 
        fact: !votes(anna, republicans)
        fact [truthValue = 0.5]: votes(anna, democrats)
        fact [truthValue = 0.666]: votes(bob, republicans)
	""")

    // anna, republicans, democrats, bob
    // Result: Map[(String, List[String]), GroundedPredicate]
    val groundedPredicates = Grounding.createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts, pslData.individualsByClass)
    val groundedRules = Grounding.createGroundedRules(pslData.rulesWithPredicates, groundedPredicates, pslData.individualsByClass)
    assert(groundedRules.size == 24)
    // TODO: add ids to rules so we can check whether it is the correct rule.
    assert(groundedRules(0).definition.variables.toSet == Set(Variable("A"), Variable("P"), Variable("B")))
    assert(groundedRules(0).body(0).definition.name == "votes")
    assert(groundedRules(0).body(0).groundings == List(anna, repub))
    assert(groundedRules(0).body(0).truthValue == Some(0.0))
    assert(groundedRules(0).body(1).definition.name == "friend")
    assert(groundedRules(0).body(1).groundings == List(anna, demo))
    assert(groundedRules(0).body(1).truthValue == None)
    assert(groundedRules(0).head(0).definition.name == "votes")
    assert(groundedRules(0).head(0).groundings == List(demo, repub))
    assert(groundedRules(0).head(0).truthValue == None)

    assert(groundedRules(1).definition.variables.toSet == Set(Variable("A"), Variable("P"), Variable("B")))
    assert(groundedRules(1).body(0).definition.name == "votes")
    assert(groundedRules(1).body(0).groundings == List(anna, repub))
    assert(groundedRules(1).body(0).truthValue == Some(0.0))
    assert(groundedRules(1).body(1).definition.name == "friend")
    assert(groundedRules(1).body(1).groundings == List(anna, bob))
    assert(groundedRules(1).body(1).truthValue == None)
    assert(groundedRules(1).head(0).definition.name == "votes")
    assert(groundedRules(1).head(0).groundings == List(bob, repub))
    assert(groundedRules(1).head(0).truthValue == Some(0.666))

    assert(groundedRules(2).definition.variables.toSet == Set(Variable("A"), Variable("P"), Variable("B")))
    assert(groundedRules(2).body(0).definition.name == "votes")
    assert(groundedRules(2).body(0).groundings == List(anna, demo))
    assert(groundedRules(2).body(0).truthValue == Some(0.5))
    assert(groundedRules(2).body(1).definition.name == "friend")
    assert(groundedRules(2).body(1).groundings == List(anna, repub))
    assert(groundedRules(2).body(1).truthValue == None)
    assert(groundedRules(2).head(0).definition.name == "votes")
    assert(groundedRules(2).head(0).groundings == List(repub, demo))
    assert(groundedRules(2).head(0).truthValue == None)

    assert(groundedRules(3).body(0).definition.name == "votes")
    assert(groundedRules(3).body(0).groundings == List(anna, demo))
    assert(groundedRules(3).body(1).definition.name == "friend")
    assert(groundedRules(3).body(1).groundings == List(anna, bob))
    assert(groundedRules(3).head(0).definition.name == "votes")
    assert(groundedRules(3).head(0).groundings == List(bob, demo))

    assert(groundedRules(4).body(0).groundings == List(anna, bob))
    assert(groundedRules(4).body(1).groundings == List(anna, repub))
    assert(groundedRules(4).head(0).groundings == List(repub, bob))
  }

  "Grounding" should " correctly ground the constraints for a small example" in {
    val pslData = PslParser.parse(""" 
        predicate[Functional]: 	votes(_,_)
        predicate[Symmetric]: 	friend( _, _)
        rule [weight = 0.5, distanceMeasure = linear]: 	votes(A,P) && friend(A, B)  => votes(B,P) 
        fact: !votes(anna, republicans)
        fact [truthValue = 0.5]: votes(anna, democrats)
        fact [truthValue = 0.666]: votes(bob, republicans)
        fact [truthValue = 0.9]: friend(bob, anna)
	""")

    // anna, democrats, republicans, bob
    // Result: Map[(String, List[String]), GroundedPredicate]
    //    val groundedPredicates = Grounding.createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts, pslData.individualsByClass)
    //    val groundedConstraints = Grounding.createGroundedConstraints(pslData.predicates, groundedPredicates, pslData.individualsByClass)
    //    //println(groundedConstraints)
    //    //assert(groundedConstraints.size == 10)
    //
    //    assert(groundedConstraints(0).property == Functional)
    //    assert(groundedConstraints(1).property == Functional)
    //    assert(groundedConstraints(2).property == Functional)
    //    assert(groundedConstraints(3).property == Functional)
    //    assert(groundedConstraints(4).property == Symmetric)
    //    assert(groundedConstraints(5).property == Symmetric)
    //    assert(groundedConstraints(6).property == Symmetric)
    //    assert(groundedConstraints(7).property == Symmetric)
    //    assert(groundedConstraints(8).property == Symmetric)
    //    assert(groundedConstraints(9).property == Symmetric)
    //
    //    assert(groundedConstraints(0).groundedPredicates.size == 3)
    //    assert(groundedConstraints(1).groundedPredicates.size == 3)
    //    assert(groundedConstraints(2).groundedPredicates.size == 3)
    //    assert(groundedConstraints(3).groundedPredicates.size == 3)
    //    assert(groundedConstraints(4).groundedPredicates.size == 2)
    //    assert(groundedConstraints(5).groundedPredicates.size == 2)
    //    assert(groundedConstraints(6).groundedPredicates.size == 2)
    //    assert(groundedConstraints(7).groundedPredicates.size == 2)
    //    assert(groundedConstraints(8).groundedPredicates.size == 2)
    //    assert(groundedConstraints(9).groundedPredicates.size == 2)
    //
    //    // votes(anna, *) knowing that votes(anna, demo) = 0.5; votes(anna, repub) = 0
    //    println(groundedConstraints(0))
    //    assert(groundedConstraints(0).unboundGroundedPredicates.size == 1)
    //    assert(groundedConstraints(0).computeCoefficientMatrix.toList == List(1.0))
    //    assert(groundedConstraints(0).computeConstant == 0.5)
    //
    //    // votes(republicans, *)
    //    println(groundedConstraints(1))
    //    assert(groundedConstraints(1).unboundGroundedPredicates.size == 3)
    //    assert(groundedConstraints(1).computeCoefficientMatrix.toList == List(1.0, 1.0, 1.0))
    //    assert(groundedConstraints(1).computeConstant == 1)
    //
    //    // votes(democrats, *) 
    //    println(groundedConstraints(2))
    //    assert(groundedConstraints(2).unboundGroundedPredicates.size == 3)
    //    assert(groundedConstraints(2).computeCoefficientMatrix.toList == List(1.0, 1.0, 1.0))
    //    assert(groundedConstraints(2).computeConstant == 1)
    //
    //    // votes(bob, *) knowing votes(bob, republicans) = 0.666
    //    println(groundedConstraints(3))
    //    assert(groundedConstraints(3).unboundGroundedPredicates.size == 2)
    //    assert(groundedConstraints(3).computeCoefficientMatrix.toList == List(1.0, 1.0))
    //    assert(groundedConstraints(3).computeConstant == 0.33399999999999996)

    // friend(anna, republicans), friend(republicans,anna)
    //    println(groundedConstraints(4))
    //assert(groundedConstraints(4).unboundGroundedPredicates.size == 2)
    //[info] Grounding
    //[info] - should correctly ground the predicates for a small example
    //[info] Grounding
    //[info] - should correctly ground the rules for a small example
    //[info] Grounding
    //[info] - should correctly ground the constraints for a small example *** FAILED ***
    //[info]   List(GroundedPredicate 39: friend[ Symmetric] (anna, bob) ) had size 1 instead of expected size 2 (GroundingSpec.scala:222)
    //    assert(groundedConstraints(4).computeCoefficientMatrix.toList == List(1.0, -1.0))
    //    assert(groundedConstraints(4).computeConstant == 0)
    //    println(groundedConstraints(5))
    //    assert(groundedConstraints(5).unboundGroundedPredicates.size == 2)
    //    assert(groundedConstraints(5).computeCoefficientMatrix.toList == List(1.0, -1.0))
    //    assert(groundedConstraints(5).computeConstant == 0)
    //
    //    println(groundedConstraints(6))
    //    println(groundedConstraints(7))
    //    println(groundedConstraints(8))
    //    println(groundedConstraints(9))
    //TODO: The commented out checks above sometimes fails on Travis, but not from within Eclipse.
  }

}
