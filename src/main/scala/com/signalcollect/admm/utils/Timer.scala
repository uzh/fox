/*
 *  @author Sara Magliacane
 *  @author Philip Stutz
 *
 *  Copyright 2013-2015 University of Zurich & VU University Amsterdam
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

/**
 * Object that provides the timing of a given operation, returning the result and the elapsed time.
 */
object Timer {
  /**
   * Given an operation, returns the elapsedTime for a given operation and the result of type R.
   */
  def time[R](operation: => R): (R, Long) = {
    val startTime = System.currentTimeMillis
    val result = operation
    val elapsedTime = System.currentTimeMillis - startTime
    (result, elapsedTime)
  }
}
