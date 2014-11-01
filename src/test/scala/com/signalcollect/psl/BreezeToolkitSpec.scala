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

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.signalcollect.TestAnnouncements
import com.signalcollect.admm.optimizers.BreezeToolkit

import breeze.linalg.DenseVector
import breeze.optimize.minimize

class BreezeToolkitSpec extends FlatSpec with Matchers with TestAnnouncements {

  "Scala" should "be able to compile a class and create an instance of it at runtime" in {
    val className = "CustomFunction"

    val customFunctionString = s"""
class $className extends Function1[Double, Double] {
  def apply(x: Double) = 2 * x
}
"""
    import scala.reflect.runtime._
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val tb = cm.mkToolBox()

    val customClass = tb.eval(tb.parse(s"$customFunctionString; scala.reflect.classTag[$className].runtimeClass")).asInstanceOf[Class[Function1[Double, Double]]]
    val instance = customClass.newInstance
    val doubled = instance(1.0)
    doubled === 2.0
  }

  "BreezeToolkit" should "be able to evaluate a function instantiated at runtime" in {
    val f = BreezeToolkit.createFunction("sum(x)")
    f(DenseVector(1.0, 2.0, 3.0)) === 6.0
  }

  it should "be able to optimize a function instantiated at runtime" in {
    val f = BreezeToolkit.createFunction("""
val xCopy = x + DenseVector[Double](3.0, 0.0, 0.0)
val const = DenseVector[Double](1.0, 1.0, 1.0) 
val xDotConst = xCopy.dot(const)
xDotConst * xDotConst
""")
    val min = minimize(f, DenseVector(0.0, 0.0, 0.0))
    val evaluated = f(min)
    evaluated === 0.0 +- 1e-8
  }

}
