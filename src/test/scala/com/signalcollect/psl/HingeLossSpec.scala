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
import com.signalcollect.util.TestAnnouncements

class HingeLossSpec extends FlatSpec with Matchers with TestAnnouncements {

  "HingeLoss optimizer" should "A correctly minimize a hinge loss term, when the solution is on the hinge" in {
    val z = Array(0.2, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val constant = -0.95
    val weight = 1.0
    val stepSize = 1.0
    val expected = Array(-.125, 0.825) 
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  it should "B correctly minimize a hinge loss term, when the solution is on the hinge" in {
    val z = Array(0.3, 0.5, 0.1)
    val y = Array(0.1, 0.0, -0.05)
    val coeffs = Array(1.0, -0.5, 0.4)
    val constant = -0.15
    val weight = 1.0
    val stepSize = 0.5
    val expected = Array(0.043257, 0.528361, 0.177309)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  it should "A correctly minimize a hinge loss term, when the solution is on the zero side" in {
    val z = Array(0.3, 0.5, 0.1)
    val y = Array(0.1, 0.0, -0.05)
    val coeffs = Array(1.0, -0.5, 0.4)
    val constant = 0.0
    val weight = 2.0
    val stepSize = 0.5
    val expected = Array(0.1, 0.5, 0.2)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  it should "B correctly minimize a hinge loss term, when the solution is on the zero side" in {
    val z = Array(0.1)
    val y = Array(0.15)
    val coeffs = Array(1.0)
    val constant = 0.0
    val weight = 2.0
    val stepSize = 1.0
    val expected = Array(-0.05)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  it should "correctly minimize a hinge loss term, when the solution is on the linear side" in {
    val z = Array(0.7, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val constant = 0.0
    val weight = 1.0
    val stepSize = 1.0
    val expected = Array(0.6, 0.6)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  it should "correctly minimize a hinge loss term, when the solution is on the hinge with non-1.0 stepsize and non-0.0 dual variables" in {
    val z = Array(0.7, 0.5)
    val y = Array(0.05, 1.0)
    val coeffs = Array(1.0, -1.0)
    val constant = -0.5
    val weight = 2.0
    val stepSize = 2.0
    val expected = Array(0.0875, 0.5875)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected);
  }

  def testProblem(
    z: Array[Double],
    y: Array[Double],
    coeffs: Array[Double],
    constant: Double,
    weight: Double,
    stepSize: Double,
    expected: Array[Double]) {
    val zMap = (0 until z.length).zip(z).toMap
    val hlt = Optimizer.hingeLoss(
      stepSize,
      zMap,
      weight,
      constant,
      coeffs,
      zMap.keys.toArray,
      1)
    hlt.setY(y)
    val resultMap = hlt.optimize(zMap)
    for (i <- 0 until z.length) {
      resultMap(i) should be(expected(i) +- 5e-5)
    }
    // TODO: check also the number of iterations?
  }

}
