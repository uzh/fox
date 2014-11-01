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

import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.util.TestAnnouncements

/**
 * Small example that exploits the functional and symmetric constraints.
 */
class ExistsExample extends FlatSpec with Matchers with TestAnnouncements {
 val existsExample = """
    class Person: anna, sara, philip, fred, paul
	class Cause: accident, snow, traffic
    class Place: dubai, zurich, florida, middle-east
    class Time: today, tomorrow, yesterday
      
	predicate: 	isLate(Person, Time)
   	predicate: 	causeIsLate(Person, Cause, Place,Time)
    predicate:  isWarm(Place)
    predicate:  locatedIn(Place, Place)
   	predicate: 	in(Person, Place)
	predicate [Symmetric]: friends(Person, Person)
    predicate: True()
    predicate: noCause(Cause,Place)
   
    // Goal:
    //rule [weight = 1]: 	isLate(P,TIME) && in(P,PLACE) => EXISTS [C] causeIsLate(P,C,PLACE,TIME)
    //rule [weight = 1]: 	isLate(P,TIME) => EXISTS [C,PLACE] causeIsLate(P,C,PLACE,TIME)
    rule [weight = 1]: 	isLate(P,TIME) => EXISTS [C,PLACE] causeIsLate(P,C,PLACE,TIME) || noCause(C,PLACE)
    //rule [weight = 1]: 	isLate(P,TIME) => EXISTS [C,PLACE] causeIsLate(P,C,PLACE,TIME) || noCause(P,TIME)
   
   // Option 1: // Cannot have functional on non binary predicates.
   //rule [weight = 1]: 	isLate(P,TIME) && in(P,PLACE) => causeIsLate(P,C,PLACE,TIME)  // Wrong, all causes are true at least as much (exactly) as the person is late.   
   
   // Option 2:
   // rule [weight = 1]: 	True() => causeIsLate(P,C,PLACE,TIME)        // All causes are true, except if you're not in that place or not late.
   // rule [weight = 1]: 	!isLate(P,TIME) => !causeIsLate(P,C,PLACE,TIME)   // Wrong, all causes have to be true if you're late in that place.
   // rule [weight = 1]: 	!in(P,PLACE) => !causeIsLate(P,C,PLACE,TIME) 
   
    // Option 3:
    //rule [weight = 1]: 	isLate(P,TIME) && in(P,PLACE) => causeIsLate(P,snow, PLACE,TIME) || causeIsLate(P,accident, PLACE,TIME) || causeIsLate(P, traffic, PLACE,TIME)
    //rule [weight = 1]: 	isLate(P,TIME) => causeIsLate(P,snow, middle-east,TIME) || causeIsLate(P,accident, middle-east,TIME) || causeIsLate(P, traffic,middle-east,TIME) || causeIsLate(P,snow, dubai,TIME) || causeIsLate(P,accident, dubai,TIME) || causeIsLate(P, traffic,dubai,TIME) || causeIsLate(P,snow, zurich,TIME) || causeIsLate(P,accident, zurich,TIME) || causeIsLate(P, traffic,zurich,TIME) || causeIsLate(P,snow, florida,TIME) || causeIsLate(P,accident, florida,TIME) || causeIsLate(P, traffic,florida,TIME)
    //rule [weight = 1]: 	isLate(P,TIME) => causeIsLate(P,snow, middle-east,TIME) || causeIsLate(P,accident, middle-east,TIME) || causeIsLate(P, traffic,middle-east,TIME) || causeIsLate(P,snow, dubai,TIME) || causeIsLate(P,accident, dubai,TIME) || causeIsLate(P, traffic,dubai,TIME) || causeIsLate(P,snow, zurich,TIME) || causeIsLate(P,accident, zurich,TIME) || causeIsLate(P, traffic,zurich,TIME) || causeIsLate(P,snow, florida,TIME) || causeIsLate(P,accident, florida,TIME) || causeIsLate(P, traffic,florida,TIME) || noCause(snow, middle-east) || noCause(accident, middle-east) || noCause( traffic,middle-east) || noCause(snow, dubai) || noCause(accident, dubai) || noCause( traffic,dubai) || noCause(snow, zurich) || noCause(accident, zurich) || noCause( traffic,zurich) || noCause(snow, florida) || noCause(accident, florida) || noCause( traffic,florida) 
    //rule [weight = 1]: 	isLate(P,TIME) => causeIsLate(P,snow, middle-east,TIME) || causeIsLate(P,accident, middle-east,TIME) || causeIsLate(P, traffic,middle-east,TIME) || causeIsLate(P,snow, dubai,TIME) || causeIsLate(P,accident, dubai,TIME) || causeIsLate(P, traffic,dubai,TIME) || causeIsLate(P,snow, zurich,TIME) || causeIsLate(P,accident, zurich,TIME) || causeIsLate(P, traffic,zurich,TIME) || causeIsLate(P,snow, florida,TIME) || causeIsLate(P,accident, florida,TIME) || causeIsLate(P, traffic,florida,TIME) || noCause(P,TIME)
    
    // Add more constraints to places.
    rule [1]: isWarm(PLACE) => !causeIsLate(P,snow,PLACE,TIME)
    rule [1]: isWarm(PLACEA) && locatedIn(PLACEB, PLACEA) => isWarm(PLACEB)
    //rule [1]: friends(A, B) && in(A, P) => in(B, P)
    //rule [1]: isLate(X,yesterday) => isLate(X,today)
    //rule [1]: isLate(X,today) => isLate(X,tomorrow)
    //rule [weight = 1]: causeIsLate(P,snow, PLACE,today) => causeIsLate(P,snow, PLACE,tomorrow)
    // rule [1]: isLate(X,TIME) => isLate(X,next(TIME))
   
	fact [truthValue = 1.0]: isLate(sara, today)
   	//fact [truthValue = 0.8]: isLate(anna, yesterday)
   	//fact [truthValue = 0.8]: friends(sara, anna)
	fact [truthValue = 1.0]: in(sara, dubai)
    fact [truthValue = 1.0]: isWarm(middle-east)
    fact [truthValue = 0.5]: locatedIn(dubai, middle-east)
    fact: True()
	"""
  "ExistsExample" should "provide a solution consistent with Matlab" in {
    val pslData = PslParser.parse(existsExample)

    val config = InferencerConfig(computeObjectiveValueOfSolution = true)  
    val inferenceResults = Inferencer.runInference(pslData, config=config)

    val solution = inferenceResults.solution
    val gps = inferenceResults.idToGpMap
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    //println(inferenceResults)
    //println("Objective function value: " + objectiveFunctionVal)

    objectiveFunctionVal should be(0.0 +- 5e-4) 
  }
 }