package com.signalcollect.psl

import java.io.File
import com.signalcollect.psl.model.GroundedPredicate

object CommandLinePslInferencer extends App {

  val usage = """
    Usage: fox filename [--absEps num] [--relEps num] [--queryList "pred1, pred2"]
  """

  if (args.length <= 1) {
    println(usage)
    System.exit(-1)
  }
  val arglist = args.toList
  val it = arglist.iterator
  val tupleOfArgs = it.zip(it).toList
  val mapOfArgs = tupleOfArgs.toMap

  val pslFile = new File(mapOfArgs.get("--filename").get)
  val queryList = if (mapOfArgs.get("--queryList").isDefined) {
    val argument = mapOfArgs.get("--queryList").get
    argument.split(",").toList
  } else {
    List.empty
  }

  val config = InferencerConfig(
    absoluteEpsilon = mapOfArgs.get("--absEps").getOrElse("1e-8").toDouble,
    relativeEpsilon = mapOfArgs.get("--relEps").getOrElse("1e-3").toDouble)
  val inferenceResults = Inferencer.runInferenceFromFile(
    pslFile = pslFile,
    config = config)

  println(inferenceResults.printSelected(queryList))
  System.exit(0)

}
