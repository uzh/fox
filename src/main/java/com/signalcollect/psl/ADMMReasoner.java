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
package com.signalcollect.psl;

import java.io.Serializable;

import scala.collection.immutable.Map;

/**
 * Uses an ADMM optimization method to optimize its GroundKernels.
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 * @author Eric Norris
 */
@SuppressWarnings("serial")
public class ADMMReasoner implements Serializable {

	public ADMMReasoner(double stepSize, Map<Integer, Double> consensus) {
		this.stepSize = stepSize;
		this.z = consensus;
	}
	public double stepSize;
	public Map<Integer, Double> z;

}
