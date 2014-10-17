package com.signalcollect.psl

import java.io.File
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.psl.Inferencer

object CommandLinePslInferencer extends App {

  assert(args.size > 0, "The path to a PSL file has to be passed as an argument.")
  val pslFile = new File(args(0))
  val inferenceResults = Inferencer.runInferenceFromFile(pslFile)
  val inferences = inferenceResults.solution.results
  val gps = inferenceResults.idToGpMap
  inferences.foreach {
    case (id, truthValue) =>
      if (truthValue > 0) {
        val gp = gps(id)
        if (!gp.truthValue.isDefined) {
          println(s"$gp has truth value $truthValue")
        }
      }
  }

  System.exit(0)

}
