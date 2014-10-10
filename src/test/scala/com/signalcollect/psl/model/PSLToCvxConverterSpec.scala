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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

class PSLToCvxConverterSpec extends FlatSpec with Matchers with TestAnnouncements {

  val anna = Individual("anna")
  val bob = Individual("bob")
  val demo = Individual("democrats")
  val repub = Individual("republicans")
  
  "PSLToCvxConverter" should " correctly convert a small example" in {  
    val predicateInRule1 = new PredicateInRule("votes", List(Variable("A"), Variable("P")))
    val predicateInRule2 = new PredicateInRule("friends", List(Variable("A"), Variable("B")))
    val predicateInRule3 = new PredicateInRule("votes", List(Variable("B"), Variable("P")))
    
    val rule = new Rule(1,List[PredicateInRule] (predicateInRule1, predicateInRule2), List[PredicateInRule] (predicateInRule3), Linear, 0.7)
    
    // Create the grounded predicates for the grounded rule: [0.7] votes(anna,democrats) and friend(anna,bob) => votes(bob,democrats)
    val predicate1 = new Predicate("votes", List("_", "_"))
    val groundedPredicate1 = new GroundedPredicate(1, predicate1, List(anna, demo), Some(0.3))
    
    val predicate2 = new Predicate("friends", List("_", "_"))
    val groundedPredicate2 = new GroundedPredicate(2, predicate2, List(anna, bob), Some(0.9))
    
    val predicate3 = new Predicate("votes", List("_", "_"))
    val groundedPredicate3 = new GroundedPredicate(3, predicate3, List(bob, demo), None)
        
    val groundedrule = new GroundedRule(1, rule, List[GroundedPredicate] (groundedPredicate1, groundedPredicate2), List[GroundedPredicate] (groundedPredicate3))

    // The result should be:
    // 0.7 * max{0, votes(anna,democrats) + friend(anna,bob) - 1 - votes(bob,democrats)} = 
    // 0.7 * max{0, 0.3 + 0.9 - 1 - votes(bob,democrats)} =
    // 0.7 * max{0, [-1] votes(bob,democrats) - (-0.2)} =
    PSLToCvxConverter.toCvxFunction(groundedrule) should be ("0.7 * max (0, [-1.0] * [x3]' - -0.19999999999999996)")
  }

}
