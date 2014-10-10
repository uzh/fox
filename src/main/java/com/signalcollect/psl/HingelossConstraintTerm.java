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
 * Objective term in the form:
 * head <= max (0, coeff*x(except head) - constant)  // coeff*x includes the head
 * This means if coeff*x <= constant then head = 0
 * otherwise coeff*x - constant >= 0 can be rewritten as a linear constraint projecting coeff*x = constant
 * 
 * Used to express formulas in the form [hard] P => Q && R : 
 * inf*max(0, P - max(0, Q + R - 1))
 * P <= max(0, Q + R - 1)
 */
@SuppressWarnings("serial")
class HingelossConstraintTerm extends HyperplaneTerm {
	
	private final int headIndex;
	
	HingelossConstraintTerm(ADMMReasoner reasoner, int[] zIndices, double[] coeffs,
			double constant, int headIndex) {
		super(reasoner, zIndices, coeffs, constant);
		this.headIndex = headIndex;
	}

	@Override
	public void minimize(){
		// /* Initializes scratch data */
		double total = 0.0;		
		double head = 0.0;
		
		/*
		 * Minimizes without regard for the constraint, i.e., solves argmin
		 * stepSize/2 * \|x - z + y / stepSize \|_2^2
		 */
		for (int i = 0; i < x.length; i++) {
			x[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			if (zIndices[i] == headIndex){
				head = x[i];
			} else {
				total += coeffs[i] * x[i];
			}
		}

		/*
		 * Checks if the solution satisfies the constraint. If so, updates the
		 * local primal variables and returns.
		 * 
		 * head <= max (0, coeff*x - constant)  // coeff*x includes the head
		 */
		
		total -= constant;
		
		if (total < 0){
			// max(0, ...)
			total = 0.0;
		}
		
		if (total >= head && head >= 0){
			// condition satisfied.
			return;
		}
		
		if (head < 0 && total == 0){
			//If coeff*x - constant <= 0 then head should be zero (that's the min allowed value).
			for (int i = 0; i < x.length; i++) {
				if (zIndices[i] == headIndex){
					x[i] = 0.0;
				}				
			}
		}
	
		/*
		 * If the naive minimization didn't work, projects onto the hyperplane.
		 * coeff*x - constant >= 0
		 * 
		 */
		project();
	}
	
	public Double evaluateAt(Map<Integer, Double> xnew){
		Double total = 0.0;
		double head = 0.0;
		for (int i = 0; i < coeffs.length; i++) {
			if (xnew.contains(zIndices[i])){
				if (zIndices[i] == headIndex){
					head = xnew.apply(zIndices[i]);
					continue;
				}
				total += coeffs[i] * xnew.apply(zIndices[i]);
			}
		}

		Double absDiff = 0.0;
		Double eps = 0.01;
		
		if (total < 0){
			// max(0, ...)
			total = 0.0;
		}
		
		if (total < head){
			absDiff = head - total;
		}
	
		if (absDiff <= eps){
			return 0.0;
		}
	
//		System.out.println("Hingeloss Constraint violated by " + absDiff + "\n");
//		for (int i = 0; i < coeffs.length; i++) {
//			System.out.print(coeffs[i]); 
//			System.out.print(" * "+ zIndices[i] + " + ");
//		}
//		System.out.print(comparator  + " " + constant + "\n");
		
		return Double.MAX_VALUE;
	}	
}
