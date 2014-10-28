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

package com.signalcollect.psl.examples

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect.TestAnnouncements

import com.signalcollect.admm.Wolf
import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.model.GroundedRule
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.PSLToCvxConverter
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.parser.ParsedPslFile

import scala.annotation.tailrec

/**
 * Small example on abduction
 */
class AbductionExample extends FlatSpec with Matchers with TestAnnouncements {

  val abdExample = """
    predicate : After(_, _)
    predicate : PDF(_)
    predicate : Tex(_)
    predicate : Bib(_)
    predicate : Excel(_)
    predicate : Doc(_)
    predicate : Image(_)
    predicate : Contains(_, _)
    predicate : TextSim(_, _)
    predicate : ImageSim(_, _)
    predicate : SameMetadata(_, _)
    predicate : Text2PDF(_, _)
    predicate : Tex2PDF(_, _)
    predicate : Review(_, _)
    predicate : CopyText(_, _)
    predicate : Copy(_, _)
    predicate : Image2PDF(_, _)
    predicate : Plot(_, _)
 
    // All the similar elements with the same metadata are copies, unless they have a different type.
    rule [100]:  PDF(X) && PDF(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [100]:  Tex(X) && Tex(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [100]:  Bib(X) && Bib(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [100]:  Excel(X) && Excel(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [100]:  Doc(X) && Doc(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [100]:  Image(X) && Image(Y) && ImageSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    
   
    // If the metadata is different, but the type is the same, then it's a review. 
    rule [100]:  PDF(X) && PDF(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [100]:  Tex(X) && Tex(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [100]:  Bib(X) && Bib(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [100]:  Excel(X) && Excel(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [100]:  Doc(X) && Doc(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [100]:  Image(X) && Image(Y) && ImageSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)

    // Doc prints to pdf
    rule [80]:  Doc(X) && PDF(Y) && TextSim(Y, X)  && After(Y, X) => Text2PDF(X,Y)

//    rule [80]:  Image(X) && PDF(Y) && Image(Z) && Contains(Y,Z) && ImageSim(X,Z)  && After(Y, X) => Image2PDF(X,Y)

    rule [80]: Tex(T) && PDF(P) && TextSim(X, Y)  && After(X, Y) => Tex2PDF(T, P)
    rule [80]: Excel(X) && Image(I)  && Contains(L,X)  && Contains(L,I) && After(I,X) => Plot(X,I)
    
    // the parts have the same time as the whole.
//    rule [100] : After(X, Y) && Contains(Z, X) => After(Z, Y)
//    rule [100] : After(X, Y) && Contains(X, Z) => After(Z, Y)

    // After is transitive.
//    rule [100] : After(X, Y) && After(Y, Z) => After(X, Z)
    
    
	// docx and docx2 are copies
fact : Doc(docxx)
fact : Doc(docx)
fact : After(docx, docxx)
fact : !After(docxx, docx)
fact [truthValue = 0.9] : TextSim(docx, docxx)
fact [truthValue = 0.9] : TextSim(docxx, docx)
fact [truthValue = 0.8] : SameMetadata(docx, docxx)
fact [truthValue = 0.8]: SameMetadata(docxx, docx)

// pdf3 is a review of pdf2
fact : PDF(pdfff)
fact : PDF(pdff)
fact : After(pdfff, pdff)  
fact  [truthValue = 0.7]  : TextSim(pdfff, pdff)
fact [truthValue = 0.6]  : TextSim(pdff, pdfff)
fact [truthValue = 0.8] : !SameMetadata(pdfff, pdff)
fact [truthValue = 0.8] : !SameMetadata(pdff, pdfff)
    
// pdf2 is a review of pdf  
fact : After(pdff, pdf)  
fact [truthValue = 0.7] : TextSim(pdff, pdf)
fact [truthValue = 0.7] : TextSim(pdf, pdff)
fact : !SameMetadata(pdff, pdf)
fact : !SameMetadata(pdf, pdff)
    
// pdf is a printed docx.
fact : PDF(pdf)
fact : After(pdf, docx)  
fact  [truthValue = 0.9] : TextSim(pdf,docx)
fact  [truthValue = 0.9] : TextSim(docx, pdf)

//// tex is the input for pdf4
//fact : Tex(tex)
//fact : After(pdffour, tex)
//fact : TextSim(pdffour, tex)
//fact : TextSim(tex, pdffour)
//    
//// imgxls contains a plot of the data in xls
//fact : Image(imgxls)
//fact : Excel(xls)
//fact : Contains(labels, xls)
//fact : Contains(labels, imgxls)
//fact : After(imgxls, xls)
//fact : After(imgxls, pdf)

//// img is similar to an img2 contained in pdf
//fact : Image(img)
//fact : Image(imgg)
//fact : Contains(pdf, imgg)
//fact : ImageSim(img, imgg)
//fact : ImageSim(imgg, img)
//fact : After(pdf, img)

	"""

  "AbductionExample" should "provide a solution consistent with Matlab" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 1e-08, relativeEpsilon = 1e-03)
    val inferenceResults = Inferencer.runInferenceFromString(abdExample, config = config)

    //println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    objectiveFunctionVal should be(0.0 +- 0.05)
    //println(PSLToCvxConverter.toCvx(abdExample)) 
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }
  /**
   *
   * 02/04/2014 @ 11:17
   * --------------
   * - Execution Statistics -
   * --------------
   * # signal steps		68
   * # collect steps		68
   * Computation time		12419 milliseconds
   * Master JVM CPU time	7391 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	20230
   * # signal operations	20230
   * # vertices (add/remove)	595 (595/0)
   * # edges (add/remove)	3752 (3752/0)
   *
   * Problem solved, getting back results.
   * GroundedPredicate(174,relation: Review(_, _),List(pdff, pdf),None) has truth value 0.6854405856725959
   * GroundedPredicate(175,relation: Review(_, _),List(docx, docxx),None) has truth value 0.1507482285780839
   * GroundedPredicate(143,relation: Text2PDF(_, _),List(docx, pdf),None) has truth value 0.9
   * GroundedPredicate(203,relation: Copy(_, _),List(docx, docxx),None) has truth value 0.6854405856725956
   * GroundedPredicate(186,relation: Review(_, _),List(pdfff, pdff),None) has truth value 0.7537411428904291
   * Objective function value: 0.04239530911140588
   *
   * Not bounded:
   * --------------
   * - Execution Statistics -
   * --------------
   * # signal steps		32
   * # collect steps		32
   * Computation time		6047 milliseconds
   * Master JVM CPU time	8271 milliseconds
   * Termination reason	GlobalConstraintMet
   * # collect operations	15504
   * # signal operations	15504
   * # vertices (add/remove)	969 (969/0)
   * # edges (add/remove)	4524 (4524/0)
   *
   * GroundedPredicate 174: Review[ ] (pdff, pdf) has truth value 0.930022493508579
   * GroundedPredicate 175: Review[ ] (docx, docxx) has truth value 0.15544357488772667
   * GroundedPredicate 143: Text2PDF[ ] (docx, pdf) has truth value 0.949612018190998
   * GroundedPredicate 203: Copy[ ] (docx, docxx) has truth value 0.930022493508579
   * GroundedPredicate 186: Review[ ] (pdfff, pdff) has truth value 0.7772178744386382
   * Objective function value: 0.0
   *
   * Matlab CVX output:
   * number of iterations   = 13
   * primal objective value =  1.31879036e-09
   * dual   objective value = -3.86218628e-09
   * gap := trace(XZ)       = 5.70e-09
   * relative gap           = 5.70e-09
   * actual relative gap    = 5.18e-09
   * rel. primal infeas     = 3.22e-16
   * rel. dual   infeas     = 6.05e-15
   * norm(X), norm(y), norm(Z) = 6.5e+01, 1.6e-10, 1.9e+03
   * norm(A), norm(b), norm(C) = 6.0e+01, 6.5e+01, 1.9e+03
   * Total CPU time (secs)  = 2.73
   * CPU time per iteration = 0.21
   * termination code       =  0
   * DIMACS: 4.2e-15  0.0e+00  1.2e-13  0.0e+00  5.2e-09  5.7e-09
   * -------------------------------------------------------------------
   * ------------------------------------------------------------
   * Status: Solved
   * Optimal value (cvx_optval): +1.31879e-09
   *
   */

  "AbductionExample" should "provide a solution consistent with Matlab with tighter epsilons" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 1e-08, relativeEpsilon = 1e-08)
    val inferenceResults = Inferencer.runInferenceFromString(abdExample, config = config)
    //println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    //println("Objective function value: " + objectiveFunctionVal)

    // 238 iterations later....
    objectiveFunctionVal should be(0.0 +- 5e-04)
    //println(PSLToCvxConverter.toCvx(abdExample)) 
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }

  "AbductionExample" should "provide a solution consistent with Matlab with more lax epsilons" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 1e-05, relativeEpsilon = 1e-03)
    val inferenceResults = Inferencer.runInferenceFromString(abdExample, config = config)
    //println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    //println("Objective function value: " + objectiveFunctionVal)

    // 66 iterations later....
    objectiveFunctionVal should be(0.0 +- 6e-02)
    //println(PSLToCvxConverter.toCvx(abdExample)) 
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }
  "AbductionExample" should "provide a solution consistent with Matlab with extremely lax epsilons" in {
    val config = InferencerConfig(computeObjectiveValueOfSolution = true, absoluteEpsilon = 1e-02, relativeEpsilon = 1e-02)
    val inferenceResults = Inferencer.runInferenceFromString(abdExample, config = config)
    //println(inferenceResults)
    val objectiveFunctionVal = inferenceResults.objectiveFun.get

    //println("Objective function value: " + objectiveFunctionVal)

    // 4 iterations later....
    objectiveFunctionVal should be(0.0 +- 40)
    //println(PSLToCvxConverter.toCvx(abdExample)) 
    //println("\n"+ ConvergencePlotter.createPlotScript(solution.convergence) + "\n")
  }
}
