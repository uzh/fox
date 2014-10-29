/*
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
 * A term in the objective to be optimized by an {@link ADMMReasoner}.
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
abstract class ADMMObjectiveTerm implements Serializable {
	protected ADMMReasoner reasoner;
	public final double[] x;
	public final double[] y;
	protected final int[] zIndices;

	public ADMMObjectiveTerm(ADMMReasoner reasoner, int[] zIndices) {
		this.reasoner = reasoner;

		x = new double[zIndices.length];
		y = new double[zIndices.length];

		// TODO: check why was this used in Bach's code in the creation.
		// zMap at creation is the initialized values for each consensus var.
		// We don't have the zMap at initialization time, but we need to pass a
		// reasoner for the step size. Commented out for this reason.
		/*
		 * This loop ensures that the reasoner, when it first computes y, will
		 * keep it at 0
		 */
		for (int i = 0; i < x.length; i++) {
			if (reasoner != null && reasoner.z != null
					&& reasoner.z.contains(i))
				x[i] = reasoner.z.apply(zIndices[i]);
			else
				x[i] = 0;
		}

		this.zIndices = zIndices;
	}

	public void setZMap(Map<Integer, Double> zMap) {
		this.reasoner.z = zMap;
	}
	
	public void setStepSize(Double stepSize) {
		this.reasoner.stepSize = stepSize;
	}

	/**
	 * Updates x to the solution of <br />
	 * argmin f(x) + stepSize / 2 * \|x - z + y / stepSize \|_2^2 <br />
	 * for the objective term f(x)
	 */
	abstract public void minimize();

	/**
	 * @return this for convenience
	 */
	public ADMMObjectiveTerm updateLagrange() {
		for (int i = 0; i < y.length; i++) {
			y[i] = y[i] + reasoner.stepSize
					* (x[i] - reasoner.z.apply(zIndices[i]));
		}

		return this;
	}
	
	abstract public Double evaluateAt(Map<Integer, Double> x);
}