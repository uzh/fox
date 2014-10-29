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
 * 0 if coeffs^T * x [?] constant <br />
 * infinity otherwise <br />
 * where [?] is ==, >=, or <=
 * <p>
 * All coeffs must be non-zero.
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
@SuppressWarnings("serial")
class LinearConstraintTerm extends HyperplaneTerm {

	private final String comparator;
	private Double tolerance = 0.0;

	LinearConstraintTerm(ADMMReasoner reasoner, int[] zIndices,
			double[] coeffs, double constant, String comparator) {
		super(reasoner, zIndices, coeffs, constant);
		this.comparator = comparator;
	}

	LinearConstraintTerm(ADMMReasoner reasoner, int[] zIndices,
			double[] coeffs, double constant, String comparator,
			Double tolerance) {
		super(reasoner, zIndices, coeffs, constant);
		this.comparator = comparator;
		this.tolerance = tolerance;
	}

	@Override
	public void minimize() {
		// /* If it's not an equality constraint, first tries to minimize
		// without the constraint */
		// if (!comparator.equals("eq")) {
		//
		// /* Initializes scratch data */
		double total = 0.0;
		Double absDiff = 0.0;

		/*
		 * Minimizes without regard for the constraint, i.e., solves argmin
		 * stepSize/2 * \|x - z + y / stepSize \|_2^2
		 */
		for (int i = 0; i < x.length; i++) {
			x[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			total += coeffs[i] * x[i];
		}

		/*
		 * Checks how much the solution violates the constraint.
		 */
		if ((comparator.equals("leq") && total > constant)
				|| (comparator.equals("geq") && total < constant)
				|| (comparator.equals("eq") && total != constant)) {
			absDiff = Math.abs(total - constant);
		}
		
/*If the violation is less than a given tolerance (default = 0 ), updates the
		 * local primal variables and returns.
*/
		if (tolerance >= 0 && absDiff <= tolerance) {
			return;
		}

		/*
		 * If the naive minimization didn't work, or if it's an equality
		 * constraint, projects onto the hyperplane
		 */
		project();
	}

	public Double evaluateAt(Map<Integer, Double> xnew) {
		Double total = 0.0;
		Double absDiff = 0.0;

		for (int i = 0; i < coeffs.length; i++) {
			if (xnew.contains(zIndices[i])) {
				total += coeffs[i] * xnew.apply(zIndices[i]);
			}
		}

		// If it's not less or equal, take the diff.
		if ((comparator.equals("leq") && total > constant)
				|| (comparator.equals("geq") && total < constant)
				|| (comparator.equals("eq") && total != constant)) {
			absDiff = Math.abs(total - constant);
		}

		// Note: for negative tolerance values return the absolute diff.
		if (tolerance < 0) {
			return absDiff;
		}

		if (absDiff <= tolerance) {
			return 0.0;
		}

		// System.out.println("\n" + "Constraint violated by " + absDiff +
		// "\n");
		// for (int i = 0; i < coeffs.length; i++) {
		// System.out.print(coeffs[i]);
		// System.out.print(" * "+ zIndices[i] + ":"+ xnew.apply(zIndices[i]) +
		// " + ");
		// }
		// System.out.print(comparator + " " + constant + "\n");

		return Double.MAX_VALUE;
	}
}
