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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.psl.model._
import com.signalcollect.psl.parser._
import com.signalcollect.util.TestAnnouncements

class GroundedRulePlaygroundSpec extends FlatSpec with Matchers with TestAnnouncements {

  "GroundedRulePSL" should "be correctly converted to an OptimizableFunction" in {
    val pslData = PslParser.parse(""" 
        predicate[Functional]: 	votes(_,_)
        predicate: 	person(_)
        predicate: 	mentor(_, _)
        predicate: 	idol( _, _)
        rule [weight = 0.5, distanceMeasure = linear]: 	person(A) && votes(A,P) && idol(B,A)  => !votes(B,P) 
        //fact: !votes(anna, republicans)
        fact: person(anna)
        fact [truthValue = 0.5]: votes(anna, democrats)
        fact [truthValue = 0.666]: votes(bob, republicans)
	""")
    val groundedPredicates = Grounding.createGroundedPredicates(pslData.rulesWithPredicates, pslData.predicates, pslData.facts, pslData.individualsByClass)
    val groundedRules = Grounding.createGroundedRules(pslData.rulesWithPredicates, groundedPredicates, pslData.individualsByClass)
    def groundedPredicateToPslExpression(gp: GroundedPredicate, negated: Boolean) = {
      import SimplePslExpression._
      val exp = {
        if (gp.truthValue.isDefined) {
          Proposition(gp.truthValue.get)
        } else {
          Variable(gp.id)
        }
      }
      if (negated) {
        !exp
      } else {
        exp
      }
    }
    for (gr <- groundedRules) {
      val optFunction = gr.createOptimizableFunction(0.1)
      if (optFunction.isDefined) {
        val optimizer = optFunction.get.
          asInstanceOf[PslOptimizerWrapper].pslOptimizer.
          asInstanceOf[HingeLossTerm]
        val implicationBody: List[SimplePslExpression] = gr.body.zipWithIndex.map {
          case (groundedPredicate, index) =>
            val negated = gr.definition.body(index).negated
            groundedPredicateToPslExpression(groundedPredicate, negated)
        }
        val implicationHead: List[SimplePslExpression] = gr.head.zipWithIndex.map {
          case (groundedPredicate, index) =>
            val negated = gr.definition.head(index).negated
            groundedPredicateToPslExpression(groundedPredicate, negated)
        }
        assert(!implicationHead.isEmpty)
        import SimplePslExpression._
        val distanceToSatisfaction = {
          if (!implicationBody.isEmpty) {
            val bodyExpression = &&(implicationBody)
            val headExpression = ||(implicationHead)
            val implication = !bodyExpression || headExpression
            val distanceToSatisfaction = !implication
            distanceToSatisfaction
          } else {
            val headExpression = ||(implicationHead)
            val distanceToSatisfaction = !headExpression
            distanceToSatisfaction
          }
        }
        val constant = distanceToSatisfaction.constant
        val convertedConstantFromGroundedRule = -optimizer.constant
        assert(constant == convertedConstantFromGroundedRule,
          s"Constants have to match: Is $convertedConstantFromGroundedRule, should be $constant")
      }
    }
  }

}
