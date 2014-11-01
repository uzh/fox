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

import com.signalcollect.util.TestAnnouncements

class GroundedConstraintSpec extends FlatSpec with Matchers with TestAnnouncements {

  val anna = Individual("anna")
  val bob = Individual("bob")
  val carl = Individual("carl")
  val demo = Individual("democrats")
  val repub = Individual("republicans")
  val PCI = Individual("PCI")
  val Berlusconi = Individual("Berlusconi")
  
  "GroundedConstraint" should " correctly create a functional constraint with two grounded predicates" in {  
    // Create the constraint: votes(anna, demo) + votes(anna, repub) = 1
    
    val predicate1 = new Predicate("votes", List("_", "_"), Set(Functional))
    val groundedPredicate1 = new GroundedPredicate(1, predicate1, List (anna, demo), Some(0.3))
    val groundedPredicate2 = new GroundedPredicate(2, predicate1, List (anna, repub), None)
    val groundedConstraint = new GroundedConstraint(1,1, Functional, List[GroundedPredicate] (groundedPredicate1, groundedPredicate2))

    // Create the constraint: 0.3 + votes(anna, repub) = 1
    groundedConstraint.computeCoefficientMatrix should be (List(1.0))
    groundedConstraint.computeConstant should be (0.7)
    groundedConstraint.computeComparator should be ("eq")
    groundedConstraint.unboundGroundedPredicates should be (List(groundedPredicate2))
  }
  
  "GroundedConstraint" should " correctly create a functional constraint with more grounded predicates" in {  
    // Create the constraint: votes(anna, demo) + votes(anna, repub) + votes(anna, PCI) + votes (anna, Berlusconi) = 1
    
    val predicate1 = new Predicate("votes", List("_", "_"), Set(Functional))
    val groundedPredicate1 = new GroundedPredicate(1, predicate1, List (anna, demo), Some(0.3))
    val groundedPredicate2 = new GroundedPredicate(2, predicate1, List (anna, repub), None)
    val groundedPredicate3 = new GroundedPredicate(3, predicate1, List (anna, PCI), None)
    val groundedPredicate4 = new GroundedPredicate(4, predicate1, List (anna, Berlusconi), None)
    val groundedConstraint = new GroundedConstraint(1, 1, Functional, List[GroundedPredicate] (groundedPredicate1, groundedPredicate2, groundedPredicate3, groundedPredicate4))

    // Create the constraint: 0.3 + votes(anna, repub) + votes(anna, PCI) + votes (anna, Berlusconi) = 1
    groundedConstraint.computeCoefficientMatrix should be (List(1.0, 1.0, 1.0))
    groundedConstraint.computeConstant should be (0.7)
    groundedConstraint.computeComparator should be ("eq")
    groundedConstraint.unboundGroundedPredicates should be (List(groundedPredicate2, groundedPredicate3, groundedPredicate4))
  }
  
   "GroundedConstraint" should " correctly create a always false partial functional constraint with more grounded predicates" in {  
    // Create the constraint: votes(anna, demo) + votes(anna, repub) + votes(anna, PCI) + votes (anna, Berlusconi) <= 1
    
    val predicate1 = new Predicate("votes", List("_", "_"), Set(PartialFunctional))
    val groundedPredicate1 = new GroundedPredicate(1, predicate1, List (anna, demo), Some(0.3))
    val groundedPredicate2 = new GroundedPredicate(2, predicate1, List (anna, repub), None)
    val groundedPredicate3 = new GroundedPredicate(3, predicate1, List (anna, PCI), None)
    val groundedPredicate4 = new GroundedPredicate(4, predicate1, List (anna, Berlusconi), Some(1.0))
    val groundedConstraint = new GroundedConstraint(1, 1, PartialFunctional, List[GroundedPredicate] (groundedPredicate1, groundedPredicate2, groundedPredicate3, groundedPredicate4))

    // Create the constraint: 0.3 + votes(anna, repub) + votes(anna, PCI) + 1.0 <= 1
    //groundedConstraint.unboundGroundedPredicates should be (List(groundedPredicate2, groundedPredicate3, groundedPredicate4))
    groundedConstraint.createOptimizableFunction(1.0) should be (None)
  }
  
  
  "GroundedConstraint" should " correctly create a symmetric constraint" in {  
    // Create the constraint: votes(anna, demo) - votes(anna, repub) = 0
    
    val predicate1 = new Predicate("votes", List("_", "_"), Set(Symmetric))
    val groundedPredicate1 = new GroundedPredicate(1, predicate1, List (anna, demo), Some(0.3))
    val groundedPredicate2 = new GroundedPredicate(2, predicate1, List (anna, repub), None)

    val groundedConstraint = new GroundedConstraint(1, 1, Symmetric, List[GroundedPredicate] (groundedPredicate1, groundedPredicate2))

    // Create the constraint: 0.3 + votes(anna, repub) = 1
    groundedConstraint.computeCoefficientMatrix should be (List(1.0))
    groundedConstraint.computeConstant should be (0.3)
    groundedConstraint.computeComparator should be ("eq")
    groundedConstraint.unboundGroundedPredicates should be (List(groundedPredicate2))
  }
}
