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

import com.signalcollect.psl.Inferencer
import com.signalcollect.psl.InferencerConfig
import com.signalcollect.util.TestAnnouncements

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
 
    // Explicit negative priors for all predicates.
    rule [1]: => !After(A, B)
    rule [1]: => !PDF(A)
    rule [1]: => !Tex(A)
    rule [1]: => !Bib(A)
    rule [1]: => !Excel(A)
    rule [1]: => !Doc(A)
    rule [1]: => !Image(A)
    rule [1]: => !Contains(A, B)
    rule [1]: => !TextSim(A, B)
    rule [1]: => !ImageSim(A, B)
    rule [1]: => !SameMetadata(A, B)
    rule [1]: => !Text2PDF(A, B)
    rule [1]: => !Tex2PDF(A, B)
    rule [1]: => !Review(A, B)
    rule [1]: => !CopyText(A, B)
    rule [1]: => !Copy(A, B)
    rule [1]: => !Image2PDF(A, B)
    rule [1]: => !Plot(A, B)

    // All the similar elements with the same metadata are copies, unless they have a different type.
    rule [10]:  PDF(X) && PDF(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [10]:  Tex(X) && Tex(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [10]:  Bib(X) && Bib(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [10]:  Excel(X) && Excel(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [10]:  Doc(X) && Doc(Y) && TextSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    rule [10]:  Image(X) && Image(Y) && ImageSim(X, Y)  && After(X, Y) && SameMetadata(X, Y) => Copy(X,Y)
    
   
    // If the metadata is different, but the type is the same, then it's a review. 
    rule [10]:  PDF(X) && PDF(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [10]:  Tex(X) && Tex(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [10]:  Bib(X) && Bib(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [10]:  Excel(X) && Excel(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [10]:  Doc(X) && Doc(Y) && TextSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)
    rule [10]:  Image(X) && Image(Y) && ImageSim(X, Y)  && After(X, Y) && !SameMetadata(X, Y) => Review(X,Y)

    // Doc prints to pdf
    rule [8]:  Doc(X) && PDF(Y) && TextSim(Y, X)  && After(Y, X) => Text2PDF(X,Y)
    rule [8]:  Image(X) && PDF(Y) && Image(Z) && Contains(Y,Z) && ImageSim(X,Z)  && After(Y, X) => Image2PDF(X,Y)

    rule [8]: Tex(T) && PDF(P) && TextSim(X, Y)  && After(X, Y) => Tex2PDF(T, P)
    rule [8]: Excel(X) && Image(I)  && Contains(L,X)  && Contains(L,I) && After(I,X) => Plot(X,I)
    
    // the parts have the same time as the whole.
    rule [10] : After(X, Y) && Contains(Z, X) => After(Z, Y)
    rule [10] : After(X, Y) && Contains(X, Z) => After(Z, Y)
    rule [10]: Contains(X, Z) => !After(X, Z)
    rule [10]: Contains(X, Z) => !After(Z, X)


    // After is transitive.
    rule [10] : After(X, Y) && After(Y, Z) => After(X, Z)
    
    
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
    fact : Tex(tex)
    fact : After(pdffour, tex)
    fact : TextSim(pdffour, tex)
    fact : TextSim(tex, pdffour)
        
    //// imgxls contains a plot of the data in xls
    fact : Image(imgxls)
    fact : Excel(xls)
    fact : Contains(labels, xls)
    fact : Contains(labels, imgxls)
    fact : After(imgxls, xls)
    fact : After(imgxls, pdf)
    
    //// imgg contained in pdf is a copy of img
    fact : Image(img)
    fact : Image(imgg)
    fact : Contains(pdf, imgg)
    fact : ImageSim(img, imgg)
    fact : ImageSim(imgg, img)
    fact : After(pdf, img)

	"""

  "AbductionExample" should "provide a good solution" in {
    // TODO: Matlab solution?
    val config = InferencerConfig(computeObjectiveValueOfSolution = true)
    val inferenceResults = Inferencer.runInferenceFromString(abdExample, config = config)
    println(inferenceResults)
    //val interestingPredicates = List("Text2PDF", "Tex2PDF", "Review", "CopyText", "Copy", "Image2PDF", "Plot")
    //println(inferenceResults.printSelected(interestingPredicates))
    val objectiveFunctionVal = inferenceResults.objectiveFun.get
    objectiveFunctionVal should be(28.24 +- 0.1)
  }
}
