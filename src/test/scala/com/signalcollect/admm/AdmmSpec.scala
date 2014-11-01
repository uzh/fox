/*
 * NOTICE: The original file was changed by Philip Stutz and Sara Magliacane.
 * 
 * This file is part of the PSL software.
 * Copyright 2011-2013 University of Maryland
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.signalcollect.admm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.signalcollect.psl.Optimizer
import com.signalcollect.util.TestAnnouncements

/**
 *  weight * coeffs^T * x
 */
class AdmmSpec extends FlatSpec with Matchers with TestAnnouncements {

  "Admm" should "correctly construct a network of nodes" in {
    val z = Array(0.0, 0.0)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val weight = 1.0
    val stepSize = 1.0
    val zMap = AdmmTestHelper.createZMap(z)
    val hlt = Optimizer.linearLoss(
      stepSize,
      zMap,
      weight,
      coeffs,
      zMap.keys.toArray,
      1)
    hlt.setY(y)

    // x_1 >= 0
    val constraint11 = Optimizer.linearConstraint(
      stepSize,
      Map(AdmmTestHelper.createId(0) -> z(0)),
      "geq",
      0, // constant
      Array(1.0), // coeff
      Array(AdmmTestHelper.createId(0)), 
      10e-3,
      2)

    // x_1 <= 1
    val constraint12 = Optimizer.linearConstraint(
      stepSize,
      Map(AdmmTestHelper.createId(0) -> z(0)),
      "leq",
      1, // constant
      Array(1.0), // coeff
      Array(AdmmTestHelper.createId(0)),
      10e-3,
      3)

    // x_2 >= 0
    val constraint21 = Optimizer.linearConstraint(
      stepSize,
      Map(AdmmTestHelper.createId(1) -> z(1)),
      "geq",
      0, // constant
      Array(1.0), // coeff
      Array(AdmmTestHelper.createId(1)),
      10e-3,
      4)

    // x_2 <= 1
    val constraint22 = Optimizer.linearConstraint(
      stepSize,
      Map(AdmmTestHelper.createId(1) -> zMap(1)),
      "leq",
      1, // constant
      Array(1.0), // coeff
      Array(AdmmTestHelper.createId(1)),
      10e-3,
      5)

//    val graph = Wolf.createGraph(List(hlt, constraint11, constraint12, constraint21, constraint22))
//    // 	 Works on graphic exploration, need to check it programmatically.

//   val graph = AdmmTestHelper.createGraph(z, Map(0 -> List(hlt, constraint11, constraint12), 1 -> 
//   List(hlt, constraint21, constraint22)) , stepSize)
   }

}
