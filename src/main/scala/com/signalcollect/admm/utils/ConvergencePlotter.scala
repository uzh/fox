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

package com.signalcollect.admm.utils

import com.signalcollect.admm.AbstractGlobalAdmmConvergenceDetection
import com.signalcollect.admm.DebugLogging

/**
 * Creates the Matlab script for displaying a graph of how the convergence evolves in the iterations.
 * Given a global convergence detection object, it uses the values in the (primal|dual)(Residual|Epsilon)ForSteps.
 * Note: 
 * Primal residual is the sum of squared distances between each local variable and the corresponding global variable.
 * Dual residual is the sum of squared distances between the global variable at the previous step and the current global variable. 
 */
object ConvergencePlotter {
  
    def createPlotScript(g: Option[AbstractGlobalAdmmConvergenceDetection]): String = {
      g match {
        case Some(x) => createPlotScript(x)
        case None => ""
      }
    }
  
  def createPlotScript(g: AbstractGlobalAdmmConvergenceDetection): String = {
    // Print Matlab arrays for each of the value series we want to plot.
    var script = "primalEps = " + g.primalEpsilonForSteps.values.mkString("[", ", ", "]") + "\n"
    script += "primalRes = " + g.primalResidualForSteps.values.mkString("[", ", ", "]") + "\n"
    script += "dualEps = " + g.dualEpsilonForSteps.filter(_._1 != 0).values.mkString("[", ", ", "]") + "\n"
    script += "dualRes = " + g.dualResidualForSteps.values.mkString("[", ", ", "]") + "\n"
    // If we are plotting a convergence with the debugging on, we have also the objective values for each step.
    g match {
      case g: DebugLogging =>
        script += "obj = " + g.objectiveValueForSteps.values.mkString("[", ", ", "]") + "\n"
    }
    // The dual epsilon that is used for the dual convergence is the one from the step before.
    val steps = g.collectStepsSoFar - 1
    script += s"dualEps($steps) = dualEps(${steps - 1}) \n"
    // Create the x axis using an array from 1 to #steps.
    script += s"x = [1:1:$steps] \n"
    // Primal residual is blue.
    // Dual residual is red.
    // Primal Epsilon is blue with a dashed line.
    // Dual Epsilon is red with a dashed line.
    // The objective function (if present) is green.
    script += "figure; grid; plot(x, dualRes, 'r', x, primalRes, 'b',  x, dualEps, 'r--o', x, primalEps, 'b--o', x, obj, 'g')"
    script
  }
}