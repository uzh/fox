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
 * weight * (coeffs^T * x - constant)^2
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
@SuppressWarnings("serial")
class SquaredLinearLossTerm extends SquaredHyperplaneTerm {
	
	SquaredLinearLossTerm(ADMMReasoner reasoner, int[] zIndices, double[] coeffs,
			double constant, double weight) {
		super(reasoner, zIndices, coeffs, constant, weight);
	}
	
	@Override
	public void minimize() {
		minWeightedSquaredHyperplane();
	}
	
	public Double evaluateAt(Map<Integer, Double> xnew){
		Double total = 0.0;
		for (int i = 0; i < coeffs.length; i++) {
			if (xnew.contains(zIndices[i])){
				total += coeffs[i] * xnew.apply(zIndices[i]);
			}
		}	
		return weight * (total - constant) * (total - constant);
	}
}
