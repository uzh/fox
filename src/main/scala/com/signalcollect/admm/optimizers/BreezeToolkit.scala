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

package com.signalcollect.admm.optimizers

import java.util.concurrent.atomic.AtomicInteger

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

import breeze.linalg.DenseVector
import breeze.optimize.ApproximateGradientFunction
import breeze.optimize.DiffFunction

trait SimpleFunction extends DiffFunction[DenseVector[Double]] {
  def f(x: DenseVector[Double]): Double
  val approximateGradient = new ApproximateGradientFunction(f)
  def calculate(x: DenseVector[Double]) = approximateGradient.calculate(x)
}

object BreezeToolkit {
  import scala.reflect.runtime._
  import scala.tools.reflect.ToolBox

  private val cm = universe.runtimeMirror(getClass.getClassLoader)
  private val tb = cm.mkToolBox()

  private val classNameCounter = new AtomicInteger(0)

  def getClassString(className: String, breezeFunctionDescription: String) = s"""
class $className extends com.signalcollect.admm.optimizers.SimpleFunction {
  def f(x: breeze.linalg.DenseVector[Double]): Double = {
    import breeze.linalg._
    import breeze.numerics._
    $breezeFunctionDescription
  }
}
scala.reflect.classTag[$className].runtimeClass
"""

  def createFunction(functionString: String): DiffFunction[DenseVector[Double]] = {
    val className = s"RuntimeBreezeFunction${classNameCounter.incrementAndGet}"
    val classString = getClassString(className, functionString)
    val customClass = tb.eval(tb.parse(classString))
    val castClass = customClass.asInstanceOf[Class[DiffFunction[DenseVector[Double]]]]
    val instance = castClass.newInstance
    instance
  }

}
