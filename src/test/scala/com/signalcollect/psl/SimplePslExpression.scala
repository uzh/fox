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

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

/**
 * A representation of PSL that can easily be converted to optimizable functions.
 */
case class SimplePslExpression(
  variablesToAdd: List[Int] = List(),
  variablesToSubtract: List[Int] = List(),
  constant: Double = 0.0,
  upperBound: Boolean = false,
  lowerBound: Boolean = false) {

  def atLeastZero = constant - variablesToSubtract.size >= 0
  def atMostZero = constant + variablesToAdd.size <= 0
  def atMostOne = constant + variablesToAdd.size <= 1

  def unary_!(): SimplePslExpression = {
    SimplePslExpression(
      variablesToAdd = variablesToSubtract,
      variablesToSubtract = variablesToAdd,
      constant = 1 - constant,
      upperBound = lowerBound,
      lowerBound = upperBound)
  }

  def ->(that: SimplePslExpression): SimplePslExpression = {
    !this || that
  }

  def -(c: Double): SimplePslExpression = {
    this.copy(constant = constant - c)
  }

  def +(c: Double): SimplePslExpression = {
    this.copy(constant = constant + c)
  }

  def ||(that: SimplePslExpression): SimplePslExpression = {
    def lowerBoundsIneffective = {
      (if (lowerBound) atLeastZero else true) &&
        (if (that.lowerBound) that.atLeastZero else true)
    }
    val disjunction = SimplePslExpression(
      variablesToAdd = variablesToAdd ++ that.variablesToAdd,
      variablesToSubtract = variablesToSubtract ++ that.variablesToSubtract,
      constant = constant + that.constant,
      upperBound = true,
      lowerBound = false)
    assert(lowerBoundsIneffective,
      s"\n$this || $that:\nOne of the expressions has a problematic lower bound.\n" +
        "Disjunctions can handle upper bounds, but lower bounds are only supported" +
        "if they're ineffective.")
    disjunction
  }

  /**
   *  A conjunction can handle a lower bound constraint on one expression,
   *  if the lower bound is guaranteed to be ineffective (atLeastZero),
   *  or if the outside part of the new expression is guaranteed to be less
   *  or equal to zero.
   *
   * a && b && c = max(0, a + b + c - 2)
   *
   * Reasoning:
   * a && b && c
   * = a && (b && c)
   * = a && max(0, b + c - 1)
   * = max(0, a + max(0, b + c - 1) - 1)
   * = max(0, (a - 1) + max(0, b + c - 1))
   * = max(0, (a + b + c - 2)) // justification: a - 1 is leq 0
   *
   * As Sara put it: max{0, max{0, whatever} + x} is max{0, whatever} if x is 0 or less
   */
  def &&(that: SimplePslExpression): SimplePslExpression = {
    def ifLowerBoundThenIneffectiveOrOutsidePartNegative = {
      (if (lowerBound) atLeastZero || (that - 1).atMostZero else true) &&
        (if (that.lowerBound) that.atLeastZero || (this - 1).atMostZero else true)
    }
    def ifUpperBoundThenIneffective = {
      (if (upperBound) atMostOne else true) &&
        (if (that.upperBound) that.atMostOne else true)
    }
    val conjunction = SimplePslExpression(
      variablesToAdd = variablesToAdd ++ that.variablesToAdd,
      variablesToSubtract = variablesToSubtract ++ that.variablesToSubtract,
      constant = constant + that.constant - 1,
      upperBound = false,
      lowerBound = true)
    assert(ifLowerBoundThenIneffectiveOrOutsidePartNegative,
      s"\n$this && $that:\nOne of the expressions has a problematic lower bound.\n" +
        "A conjunction can only handle a lower bound constraint,\n" +
        "if the lower bound is guaranteed to be ineffective (atLeastZero)," +
        "or if the outside part of the new expression is guaranteed to be less or equal to zero.")
    assert(ifUpperBoundThenIneffective,
      s"\n$this && $that:\nOne of the expressions has a problematic upper bound.\n" +
        "A conjunction can only handle an upper bound constraint,\n" +
        "if the upper bound is guaranteed to be ineffective (atMostOne).")
    conjunction
  }

  def pretty: String = {
    val posString = variablesToAdd.map("var#" + _).mkString(" + ")
    val neg = variablesToSubtract.map("var#" + _).mkString(" - ")
    val negString = if (variablesToSubtract.isEmpty) {
      ""
    } else if (variablesToAdd.isEmpty) {
      "-" + neg
    } else {
      " - " + neg
    }
    val constantString = {
      if (constant == 0) {
        ""
      } else if (constant < 0 && (!variablesToAdd.isEmpty || !variablesToSubtract.isEmpty)) {
        " - " + math.abs(constant)
      } else if (!variablesToAdd.isEmpty || !variablesToSubtract.isEmpty) {
        " + " + constant
      } else {
        constant
      }
    }
    val mainResult = s"$posString$negString$constantString"
    def max(s: String) = s"max(0, $s)"
    def min(s: String) = s"min(1, $s)"
    val maxString = if (lowerBound) max(mainResult) else mainResult
    val minString = if (upperBound) min(maxString) else maxString
    minString
  }
}

object SimplePslExpression {
  def &&(p: List[SimplePslExpression]) = {
    def recAnd(l: List[SimplePslExpression]): SimplePslExpression = {
      l match {
        case Nil => throw new Exception("And is not defined when there are no arguments.")
        case a :: Nil => a
        case a :: tail => a && recAnd(tail)
      }
    }
    recAnd(p)
  }
  def ||(p: List[SimplePslExpression]) = {
    def recOr(l: List[SimplePslExpression]): SimplePslExpression = {
      l match {
        case Nil => throw new Exception("Or is not defined when there are no arguments.")
        case a :: Nil => a
        case a :: tail => a || recOr(tail)
      }
    }
    recOr(p)
  }
  def Variable(name: Int) = SimplePslExpression(variablesToAdd = List(name))
  def Proposition(truthValue: Double) = SimplePslExpression(constant = truthValue)
}
