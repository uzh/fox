package com.signalcollect.psl

import java.io.File
import com.signalcollect.psl.model.GroundedPredicate

object CommandLinePslInferencer extends App {

  val usage = """
    Usage: fox filename [--abseps num] [--releps num] 
  """

  if (args.length == 0) {
    println(usage)
    System.exit(-1)
  }
  val arglist = args.toList
  val it = arglist.iterator
  val tupleOfArgs = it.zip(it).toList
  val mapOfArgs = tupleOfArgs.toMap

  val pslFile = new File(mapOfArgs.get("--filename").get)
  val config = InferencerConfig(
    absoluteEpsilon = mapOfArgs.get("--abseps").getOrElse("1e-8").toDouble,
    relativeEpsilon = mapOfArgs.get("--releps").getOrElse("1e-3").toDouble)
  val inferenceResults = Inferencer.runInferenceFromFile(
    pslFile = pslFile,
    config = config)
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
