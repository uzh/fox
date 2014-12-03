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
import com.signalcollect.util.TestAnnouncements

class MovieRecommendationsWithClasses extends FlatSpec with Matchers with TestAnnouncements {

  val movieExample2 = """
class User: sara, philip, fred, george
class Movie: mission-impossible, rear-window, vertigo, top-gun, blade-runner, indiana-jones-and-the-last-crusade, star-wars, zelig, pulp-fiction
class Actor: john-travolta, samuel-l-jackson, ewan-mcgregor, tom-cruise, woody-allen
class Director: woody-allen, quentin-tarantino, ridley-scott
class Likable: woody-allen, quentin-tarantino, ridley-scott, john-travolta, samuel-l-jackson, ewan-mcgregor, tom-cruise, mission-impossible, rear-window, vertigo, top-gun, blade-runner, indiana-jones-and-the-last-crusade, star-wars, zelig, pulp-fiction

predicate [prior = 0.5]: likes(User, Likable)
predicate [prior = 0]: playsIn(Actor, Movie)
predicate [Functional]: directedBy(Movie, Director)
predicate [Symmetric]: friends(User, User)

// Each director directed at least a movie.
rule [weight = 1000]: EXISTS[MOVIE] directedBy(MOVIE, DIRECTOR)
rule [weight = 5]:   playsIn(ACTOR-DIRECTOR,MOVIE) => directedBy(MOVIE, ACTOR-DIRECTOR)

// Like an actor, like movie with the actor.
rule [weight = 5]:   likes(USER-A, ACTOR) && playsIn(ACTOR,MOVIE) => likes(USER-A,MOVIE)
rule [weight = 1]:   likes(USER-A, MOVIE) && playsIn(ACTOR,MOVIE) => likes(USER-A,ACTOR)

// Like a director, like the movie with the director.
rule [weight = 2]:   likes(USER-A, DIRECTOR) && directedBy(MOVIE, DIRECTOR) => likes(USER-A,MOVIE)
rule [weight = 10]:   likes(USER-A,MOVIE) && directedBy(MOVIE,DIRECTOR) => likes(USER-A,DIRECTOR)
// Like what your friends like.
// likesAL + friendsAB - 1 - likesBL
// likesAL = 0.9 => friendsAB - likesBL - 0.1 => friendsAB <= likesBL + 0.1
rule [weight = 1]:   likes(USER-A, LIKABLE) && friends(USER-A,USER-B) => likes(USER-B,LIKABLE)
// Like what similar users like, if they like at least two similar things.
rule [weight = 0.1]: likes(USER-A, LIKABLE) && likes(USER-A, LIKABLE2) && likes(USER-B,LIKABLE)  => likes(USER-B,LIKABLE2)
 
fact: playsIn(tom-cruise, top-gun)
fact: playsIn(tom-cruise, mission-impossible)
fact: playsIn(woody-allen, zelig)
fact: playsIn(samuel-l-jackson, pulp-fiction)
fact: directedBy(pulp-fiction, quentin-tarantino)
fact: !playsIn(samuel-l-jackson, zelig)

fact [truthValue = 0.9]: friends(sara, philip)
fact [truthValue = 0.1]: likes(sara, tom-cruise)
fact [truthValue = 0.8]: likes(sara, pulp-fiction)

fact [truthValue = 0.9]: likes(philip, pulp-fiction)
fact [truthValue = 0.9]: likes(fred, tom-cruise)
	"""

  "MovieRecommendationsWithClasses" should "correctly minimize the simplified movie example" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(movieExample2, config = config)
    val objectiveFunctionValOption = inferenceResults.objectiveFun
    assert(objectiveFunctionValOption.isDefined)
  }

}
