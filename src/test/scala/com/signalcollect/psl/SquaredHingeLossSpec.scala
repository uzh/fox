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
import com.signalcollect.admm.optimizers.SquaredHingeLossOptimizer
import com.signalcollect.TestAnnouncements

class SquaredHingeLossSpec extends FlatSpec with Matchers with TestAnnouncements {

  "SquaredHingeLoss optimizer" should "A correctly minimize a squared hinge loss term, when the solution is on the quadratic side" in {
    val z = Array(0.2, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val constant = -0.95
    val weight = 1.0
    val stepSize = 1.0
    val expected = Array(-0.06, 0.76)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }

  it should "B correctly minimize a squared hinge loss term, when the solution is on the quadratic side" in {
    val z = Array(0.3, 0.5, 0.1)
    val y = Array(0.1, 0.0, -0.05)
    val coeffs = Array(1.0, -0.5, 0.4)
    val constant = -0.15
    val weight = 1.0
    val stepSize = 0.5
    val expected = Array(0.051798, 0.524096, 0.180720)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }

  it should "A correctly minimize a squared hinge loss term, when the solution is on the zero side" in {
    val z = Array(0.3, 0.5, 0.1)
    val y = Array(0.1, 0.0, -0.05)
    val coeffs = Array(1.0, -0.5, 0.4)
    val constant = 0.0
    val weight = 2.0
    val stepSize = 0.5
    val expected = Array(0.1, 0.5, 0.2)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }

  it should "C correctly minimize a squared hinge loss term, when the solution is on the quadratic side" in {
    val z = Array(0.1)
    val y = Array(-0.15)
    val coeffs = Array(1.0)
    val constant = 0.0
    val weight = 2.0
    val stepSize = 1.0
    val expected = Array(0.05)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }

  it should "D correctly minimize a squared hinge loss term, when the solution is on the quadratic side" in {
    val z = Array(0.7, 0.5)
    val y = Array(0.0, 0.0)
    val coeffs = Array(1.0, -1.0)
    val constant = 0.0
    val weight = 1.0
    val stepSize = 1.0
    val expected = Array(0.62, 0.58)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }

  it should "correctly minimize a squared hinge loss term, when the solution is on the quadratic side. Tests factorization caching by repeating the test three times" in {
    val z = Array(3.7, -0.5, 0.5)
    val y = Array(0.0, 0.0, 0.0)
    val coeffs = Array(1.0, -1.0, 0.5)
    val constant = -0.5
    val weight = 2.0
    val stepSize = 2.0
    val expected = Array(1.9, 1.3, -0.4)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
  }
  
  "The alternative optimizer" should "produce the same result as the reference optimiser" in {
    val z = Array(0.0)
    val y = Array(0.0)
    val coeffs = Array(-1.0)
    val constant = -1.0
    val weight = 1.0
    val stepSize = 1.0
    val expected = Array(0.6666666)
    testProblem(z, y, coeffs, constant, weight, stepSize, expected)
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
    // Instantiate the reasoner, which contains all the grounded rules and seems to act as a Context.
    val hlt = Optimizer.squaredHingeLoss(
      stepSize,
      zMap,
      weight,
      constant,
      coeffs,
      zMap.keys.toArray,
      1)
    hlt.setY(y)

    val ownOptimizer = new SquaredHingeLossOptimizer(
      1,
      weight = weight,
      constant = constant,
      zIndices = zMap.keys.toArray,
      stepSize = stepSize,
      initialZmap = zMap,
      coefficientMatrix = coeffs)
    ownOptimizer.setY(y)

    val resultMap = hlt.optimize(zMap)
    val ownResultMap = ownOptimizer.optimize(zMap)

    for (i <- 0 until z.length) {
      resultMap(i) should be(expected(i) +- 5e-5)
      ownResultMap(i) should be(expected(i) +- 5e-5)
    }
    //    println("Ours: Evaluation at result: " + ownOptimizer.evaluateAt(ownResultMap))
    //    println("Ours: Gradient at result: " + ownOptimizer.gradientAt(ownResultMap))
    //    println("Theirs: Evaluation at result: " + ownOptimizer.evaluateAt(resultMap))
    //    println("Theirs: Gradient at result: " + ownOptimizer.gradientAt(resultMap))
    // TODO: check also the number of iterations?
  }

}
