package com.signalcollect.psl

import java.io.File
import com.signalcollect.psl.model.GroundedPredicate

object CommandLinePslInferencer extends App {

  assert(args.size > 0, "The path to a PSL file has to be passed as an argument.")
  val pslFile = new File(args(0))
  val inferenceResults = Inferencer.runInferenceFromFile(
    pslFile = pslFile,
    config = InferencerConfig(
      absoluteEpsilon = 1e-5,
      relativeEpsilon = 1e-3))
  val inferences = inferenceResults.solution.results
  val gps = inferenceResults.idToGpMap
  def reportInference(gpId: Int, truthValue: Double) {
    if (truthValue > 0) {
      val gp = gps(gpId)
      if (!gp.truthValue.isDefined) {
        println(s"$gp has truth value $truthValue")
      }
    }
  }
  inferences.foreach(reportInference)

  System.exit(0)

}
