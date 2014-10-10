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

class LogicPuzzle {

  // From: http://www.mathsisfun.com/logicpuzzle.html

  val puzzle = """
predicate: neighbour(_)
predicate: house(_)
predicate: colour(_)
predicate: pet(_)
predicate: sport(_)
predicate: nationality(_)
predicate: beverage(_)    
predicate [Functional, InverseFunctional]: toTheLeftOf(_, _)
predicate [Functional, InverseFunctional]: toTheRightOf(_, _)
predicate [Functional, InverseFunctional]: hasColour(_, _)
predicate [Functional, InverseFunctional]: ownsPet(_, _)
predicate [Functional, InverseFunctional]: playsSport(_, _)
predicate [Functional, InverseFunctional]: hasNationality(_, _)
predicate [Functional, InverseFunctional]: drinksBeverage(_, _)
   
rule: hasNationality(X, british) <=> hasColour(X, red)
rule: hasNationality(X, swedish) <=> ownsPet(X, dogs)
rule: hasNationality(X, danish) <=> drinksBeverage(X, tea)
rule: hasColour(X, green) && hasColour(Y, white) => toTheLeftOf(X, Y)
rule: hasColour(X, green) <=> drinksBeverage(X, coffee)
rule: playsSport(X, polo) <=> ownsPet(X, birds)
rule: hasColour(X, yellow) <=> playsSport(X, hockey)
rule: playsSport(X, baseball) && ownsPet(Y, cats) => toTheLeftOf(Y, X) || toTheRightOf(Y, X)
rule: ownsPet(X, horses) && playsSport(Y, hockey) => toTheLeftOf(Y, X) || toTheRightOf(Y, X)
rule: drinksBeverage(X, beer) <=> playsSport(X, billiards)
rule: hasNationality(X, german) <=> playsSport(X, soccer)
rule: hasNationality(X, norwegian) && hasColour(Y, blue) <=> toTheLeftOf(Y, X) || toTheRightOf(Y, X)
rule: playsSport(X, baseball) && drinksBeverage(Y, water) <=> toTheLeftOf(Y, X) || toTheRightOf(Y, X)
       
fact: drinksBeverage(3, milk)
fact: hasNationality(1, norwegian)

fact: house(1)
fact: house(2)
fact: house(3)
fact: house(4)
fact: house(5)
fact: toTheLeftOf(1, 2)
fact: toTheLeftOf(2, 3)
fact: toTheLeftOf(3, 4)
fact: toTheLeftOf(4, 5)
fact: toTheRightOf(2, 1)
fact: toTheRightOf(3, 2)
fact: toTheRightOf(4, 3)
fact: toTheRightOf(5, 4)
fact: colour(red)
fact: colour(yellow)
fact: colour(blue)
fact: colour(green)
fact: colour(white)
fact: pet(fishes)
fact: pet(dogs)
fact: pet(birds)
fact: pet(cats)
fact: pet(horses)
fact: sport(polo)
fact: sport(hockey)
fact: sport(baseball)
fact: sport(billiards)
fact: sport(soccer)
fact: nationality(british)
fact: nationality(swedish)
fact: nationality(danish)
fact: nationality(norwegian)
fact: nationality(german)
fact: beverage(tea)
fact: beverage(coffee)
fact: beverage(milk)
fact: beverage(beer)
fact: beverage(water)   
"""

}
