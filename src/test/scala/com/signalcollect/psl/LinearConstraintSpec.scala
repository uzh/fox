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
package com.signalcollect.psl

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

class LinearConstraintSpec extends FlatSpec with Matchers with TestAnnouncements {

  "LinearConstraint optimizer" should " correctly minimize, when the constraint is inactive at solution" in {
    val z = Array(0.2, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, 1.0)
    val constant = 1.0
    val comparator = "leq"
    val stepSize = 1.0
    val expected = Array(0.2, 0.5)
    testProblem(z, y, coeffs, constant, comparator, stepSize, expected);
  }

  it should " correctly minimize, when the constraint is active at solution" in {
    val z = Array(0.7, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, 1.0)
    val constant = 1.0
    val comparator = "leq"
    val stepSize = 0.5
    val expected = Array(0.6, 0.4)
    testProblem(z, y, coeffs, constant, comparator, stepSize, expected);
  }

  it should " correctly minimize an equality constraint" in {
    val z = Array(0.7, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val constant = 0.0
    val comparator = "eq"
    val stepSize = 1.0
    val expected = Array(0.6, 0.6)
    testProblem(z, y, coeffs, constant, comparator, stepSize, expected);
  }

  def testProblem(
    z: Array[Double],
    y: Array[Double],
    coeffs: Array[Double],
    constant: Double,
    comparator: String,
    stepSize: Double,
    expected: Array[Double]) {
    val zMap = (0 until z.length).zip(z).toMap
    val hlt = Optimizer.linearConstraint(
      stepSize,
      zMap,
      comparator,
      constant,
      coeffs,
      zMap.keys.toArray,
      0.0,
      1)
    hlt.setY(y)
    val resultMap = hlt.optimize(zMap)
    for (i <- 0 until z.length) {
      resultMap(i) should be(expected(i) +- 5e-5)
    }
  }

}
