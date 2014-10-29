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

package com.signalcollect.psl.examples

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

import com.signalcollect.admm.Wolf
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.Grounding
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.PSLToCvxConverter
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile

import scala.annotation.tailrec

/**
 * Small example that exploits the functional and symmetric constraints.
 */
class ClassesExample extends FlatSpec with Matchers with TestAnnouncements {

  val basicExample = """
	  class Person: bob, sara
	  class Party: demo
	  predicate[PartialFunctional]: votes(Person, Party)
	  predicate [Symmetric]: 		friends(Person, Person)
	  rule [weight = 1]: 	votes(A,P) && friends(A,B) => votes(B,P) 
  	fact: friends(anna, bob)
  	fact [truthValue = 0.6]: votes(anna, demo)
    fact [truthValue = 0.1]: votes(anna, repub)
    individuals: carl
"""

  val equivalentWithoutClassesExample = """
  	predicate[PartialFunctional]: 	votes(_, _)
  	predicate [Symmetric]: 		friends(_, _)
  	predicate: Person(_)
    predicate: Party(_)

    rule [weight = 1]: 	Person(A) && Person(B) && Party(P) && votes(A,P) && friends(A,B) => votes(B,P)
    rule [100]: 	Person(A) => !Party(A) 
    rule [100]: 	Party(A) => !Person(A)  
    rule [100]: friends(A, B) => Person(A)
    rule [100]: friends(A, B) => Person(B)
    rule [100]: votes(B, P) => Person(B) 
    rule [100]: votes(B, P) => Party(P) 

  	fact: friends(anna, bob)
  	fact [truthValue = 0.6]: votes(anna, demo)
    fact [truthValue = 0.1]: votes(anna, repub)
    fact: friends(sara, anna)
    
    individuals: carl
	"""
  
  "ClassesExample" should "infer the proper classes" in {
    val pslData = PslParser.parse(basicExample)
    val config = InferencerConfig(objectiveLoggingEnabled = true, 
        absoluteEpsilon = 1e-8, relativeEpsilon = 1e-3, isBounded = true, removeSymmetricConstraints = true)
    val inferenceResults = Inferencer.runInference(pslData, config = config)
    println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    objectiveFunctionVal should be(0.0 +- 0.05)
  }

}
