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

import java.util.Arrays;

import scala.collection.immutable.Map;

/**
 * {@link ADMMReasoner} objective term of the form <br />
 * weight * [max(coeffs^T * x - constant, 0)]^2
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
@SuppressWarnings("serial")
class SquaredHingeLossTerm extends SquaredHyperplaneTerm {

	public SquaredHingeLossTerm(ADMMReasoner reasoner, int[] zIndices,
			double[] coeffs, double constant, double weight) {
		super(reasoner, zIndices, coeffs, constant, weight);
	}

	@Override
	public void minimize() {
		/* Initializes scratch data */
		double total = 0.0;

		/*
		 * Minimizes without the quadratic loss, i.e., solves argmin stepSize/2
		 * * \|x - z + y / stepSize \|_2^2
		 */
		for (int i = 0; i < x.length; i++) {
			x[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			total += coeffs[i] * x[i];
		}

		/*
		 * If the quadratic loss is NOT active at the computed point, it is the
		 * solution...
		 */
		if (total <= constant) {
			return;
		}

		/*
		 * Else, minimizes with the quadratic loss, i.e., solves argmin weight *
		 * (coeffs^T * x - constant)^2 + stepSize/2 * \|x - z + y / stepSize
		 * \|_2^2
		 */
		minWeightedSquaredHyperplane();
	}

	public Double evaluateAt(Map<Integer, Double> xnew) {
		Double total = 0.0;
		for (int i = 0; i < coeffs.length; i++) {
			if (xnew.contains(zIndices[i])) {
				total += coeffs[i] * xnew.apply(zIndices[i]);
			}
		}
		if (total <= constant) {
			return 0.0;
		}
		return weight * (total - constant) * (total - constant);
	}

	public String toString() {
		Double[] z = new Double[zIndices.length];
		for (int i = 0; i < zIndices.length; i++) {
			int zIndex = zIndices[i];
			z[i] = reasoner.z.apply(zIndex);
		}
		String s = "SquaredHingeLossTerm(x=" + Arrays.toString(x) + ", y="
				+ Arrays.toString(y) + ", z=" + Arrays.toString(z)
				+ ", coeffs=" + Arrays.toString(coeffs) + ", constant="
				+ constant + ", zIndices=" + Arrays.toString(zIndices) + ")";
		return s;
	}
}
