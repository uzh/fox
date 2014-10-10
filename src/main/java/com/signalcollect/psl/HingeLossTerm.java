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

import scala.collection.immutable.Map;

/**
 * {@link ADMMReasoner} objective term of the form <br />
 * weight * max(coeffs^T * x - constant, 0)
 * <p>
 * All coeffs must be non-zero.
 * 
 * Modified for using with S/C and added more comments. Hinge loss is a type of
 * convex loss function used in training classifiers (in particular SVMs). In
 * the case of linear SVMs it represents a function in the form: max (0, 1 - w*
 * x - b) which is not differentiable, but has a subgradient in w_i the form: if
 * (w*x -b < 1) ? x_i : 0
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
@SuppressWarnings("serial")
public class HingeLossTerm extends HyperplaneTerm implements
		WeightedObjectiveTerm {

	private double weight;

	public HingeLossTerm(ADMMReasoner reasoner, int[] zIndices,
			double[] coeffs, double constant, double weight) {
		super(reasoner, zIndices, coeffs, constant);
		setWeight(weight);
	}

	@Override
	public void setWeight(double weight) {
		this.weight = weight;
	}

	/**
	 * Update the value of x to: argmin f(x) + stepSize / 2 * \|x - z + y /
	 * stepSize \|_2^2
	 */
	@Override
	public void minimize() {
		/* Initializes scratch data */
		double total = 0.0;

		/*
		 * Minimizes without the linear loss, i.e., solves argmin stepSize/2 *
		 * \|x - z + y / stepSize \|_2^2 The derivative in x is: stepSize * (x -
		 * z + y/stepSize) 
		 * The min is the zero of the derivative. 
		 * N.B. you can minimize like this, because you may get lucky and the w*x < const
		 * with the x you find.
		 */
		for (int i = 0; i < x.length; i++) {
			// x_i = z_i (i.e. consensus value of x_i) - (y_i/ rho (i.e.
			// stepSize))
			x[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			total += coeffs[i] * x[i];
		}

		/*
		 * If the linear loss is NOT active at the computed point, it is the
		 * solution... In other terms, if the result of coeff*x is smaller than
		 * the constant, the subtraction will result in a value lower than 0,
		 * therefore it will be cut by max{0,.}. So the current values of x are
		 * already the minimal.
		 */
		if (total <= constant) {
			return;
		}
		// If total> constant, then max{0, .} is not activated and we still need
		// to minimize.

		/*
		 * Else, minimizes with the linear loss, i.e., solves 
		 * argmin weight * coeffs^T * x + stepSize/2 * \|x - z + y / stepSize \|_2^2 
		 * The derivative in x is: 
		 * stepSize * (x - z + y/stepSize) + w * coeff 
		 * The min is the zero of the derivative. 
		 * N.B. this would be 
		 * argmin weight * coeffs^T * x - weight *constant + stepSize/2 * \|x - z + y / stepSize
		 * \|_2^2 but the constant does not influence the minimization.
		 */
		total = 0.0;
		for (int i = 0; i < x.length; i++) {
			// x_i = z_i (i.e. consensus value of x_i) - (y_i/ rho (i.e.
			// stepSize)) - weight * coeff_i/ rho
			x[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			x[i] -= weight * coeffs[i] / reasoner.stepSize;
			// total = coeffs^T * x
			total += coeffs[i] * x[i];
		}

		/*
		 * If the linear loss IS active at the computed point, it is the
		 * solution...
		 */
		if (total >= constant) {
			return;
		}

		/*
		 * Else, the solution is on the hinge. Finds the orthogonal projection
		 * onto the hyperplane argmin stepSize/2 * \|x - z + y / stepSize \|_2^2
		 * such that coeffs^T * x = constant
		 */
		project();
	}
	
	public Double evaluateAt(Map<Integer, Double> xnew){
		Double total = 0.0;
		for (int i = 0; i < coeffs.length; i++) {
			if (xnew.contains(zIndices[i])){
				total += coeffs[i] * xnew.apply(zIndices[i]);
			}
		}	
		if (total <= constant) {
			return 0.0;
		}
		return weight * (total - constant);
	}
}
