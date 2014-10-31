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

/**
 * Objective term for an {@link ADMMReasoner} that is based on a hyperplane in some way.
 * <p>
 * Stores the characterization of the hyperplane as coeffs^T * x = constant
 * and projects onto the hyperplane.
 * <p>
 * All coeffs must be non-zero.
 * 
 * @author Stephen Bach <bach@cs.umd.edu>
 */
@SuppressWarnings("serial")
abstract class HyperplaneTerm extends ADMMObjectiveTerm {
	
	protected final double[] coeffs;
	protected final double constant;
	protected final double[] unitNormal;
	
	HyperplaneTerm(ADMMReasoner reasoner, int[] zIndices, double[] coeffs, double constant) {
		super(reasoner, zIndices);
		
		this.coeffs = coeffs;
		this.constant = constant;
		
		if (x.length >= 3) {
			/* 
			 * Finds a unit vector normal to the hyperplane and a point in the
			 * hyperplane for future projections.
			 * A normal vector to the hyperplane coeff*x = constant would be [coeff].
			 * The distance of a point x0 to the plane is: 
			 * |coeff*x0 - constant|/|coeff|_2
			 *
			 */
			double length = 0.0;
			for (int i = 0; i < coeffs.length; i++)
				length += coeffs[i] * coeffs[i];
			// length is the Euclidean norm (L2 norm) of the vector.
			length = Math.sqrt(length);
			
			// unitNormal contains the coefficients normalized by the length.
			unitNormal = new double[coeffs.length];
			for (int i = 0; i< unitNormal.length; i++)
				unitNormal[i] = coeffs[i] / length;
		}
		else
			unitNormal = null;
	}
	
	/**
	 * Finds the orthogonal projection onto the hyperplane <br />
	 * argmin stepSize/2 * \|x - z + y / stepSize \|_2^2 <br />
	 * such that coeffs^T * x = constant
	 * <p>
	 * Stores the result in x.
	 */
	protected void project() {
		if (x.length == 1) {
			x[0] = constant / coeffs[0];
		}
		else if (x.length == 2) {
			/*
			 * argmin \rho/2 * |x - z + y/\rho|_2^2 
			 * s.t. c_0 x_0 + c_1 x_1 = constant
			 * 
			 * argmin \rho/2 * [ (x_0 - z_0 + y_0/\rho)^2 + (x_1 - z_1 + y_1/\rho)^2 ]
			 * s.t. c_0 x_0 + c_1 x_1 = constant
			 *  
			 * x_0 = z_0 - y_0/\rho 
			 * x_0 -= c_0/c_1 * (- constant/c_1 + z_1 - y_1/rho)
			 * x_0 /= (1 + c_0^2/ c_1^2) 
			 * 
			 * According to the x.length > 2 solution, this should be:
			 * x_0 = z_0 - y_0/\rho 
			 * x_0 -= c_0/c_1 * [constant/c_1 + z_1 - y_1/rho ]
			 * > x_0 -= (c_0/c_1)^2 * (z_0 - y_0/\rho)
			 * x_0 /= (1 + c_0^2/ c_1^2) 
			 */
			x[0] =  reasoner.z.apply(zIndices[0]) - y[0]/reasoner.stepSize;
			x[0] -= (coeffs[0] / coeffs[1]) * ((-1 * constant / coeffs[1]) + reasoner.z.apply(zIndices[1]) - y[1]/reasoner.stepSize);
			// TODO(sara): check if the following is necessary.
			// x[0] -= (coeffs[0] / coeffs[1]) * (reasoner.z.apply(zIndices[0]) - y[0]/reasoner.stepSize);
			x[0] /= (1 + coeffs[0] * coeffs[0] / (coeffs[1] * coeffs[1]));
			
			// Satisfy the constraint: coeff_0 * x_0 + coeff_1 * x_1 = constant
			x[1] = (constant - coeffs[0] * x[0]) / coeffs[1];
		}
		else {
			double[] point = new double[x.length];
			for (int i = 0; i < x.length; i++)
				point[i] = reasoner.z.apply(zIndices[i]) - y[i] / reasoner.stepSize;
			
			/* For point (constant / coeffs[0], 0,...) in hyperplane dotted with unitNormal
			 * unitNormal contains the coefficients normalized by the L2 norm of the vector.
			 * 
			 * distance(point)=|coeff*point -constant |/|coeff|_2 
			 * 
			 * x = z - y/\rho - unit normal vector * distance (z_i-y_i) from the hyperplane
			 * multiplier = - constant / length 
			 */
			double multiplier = (-1 * constant / coeffs[0]) * unitNormal[0];
			
			for (int i = 0; i < x.length; i++)
				multiplier += point[i] * unitNormal[i];
			
			/* x_i = z_i - y_i/\rho  
			*  x_i -=  (\Sigma_j(-constant/length + coeff_j * (z_j - y_j/\rho)/length)) * coeff_i/ length 
			*  x_i -=  (\Sigma_j(-constant + coeff_j * (z_j - y_j/\rho)) * coeff_i/ length^2 
			*  x_i -=  (\Sigma_j(-constant + coeff_j * (z_j - y_j/\rho)) * coeff_i/ (\Sigma_j coeff_j^2)
			*/
			for (int i = 0; i < x.length; i++)
				x[i] = point[i] - multiplier * unitNormal[i];
		}
	}
}
