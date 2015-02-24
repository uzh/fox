package com.signalcollect.psl

import java.io.File
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.admm.utils.MinimaExplorer

object CommandLinePslInferencer extends App {

  val usage = """
    Usage: fox filename [--absEps num] [--relEps num] [--queryList "pred1, pred2"] [--multipleMinima true] [--threeValuedLogic true]
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
  val queryList = mapOfArgs.get("--queryList") match {
    case Some(query) =>
      query.split(",").toList
    case None =>
      List.empty
  }

  val config = InferencerConfig(
    lazyThreshold = None,
    removeSymmetricConstraints = false,
    maxIterations = 200000,
    tolerance = 0,
    absoluteEpsilon = mapOfArgs.get("--absEps").getOrElse("1e-8").toDouble,
    relativeEpsilon = mapOfArgs.get("--relEps").getOrElse("1e-5").toDouble)

  if (!mapOfArgs.get("--multipleMinima").isDefined) {
    val inferenceResults = Inferencer.runInferenceFromFile(
      pslFile = pslFile,
      config = config)
    println(inferenceResults.printSelected(queryList))
  } else {
    val results = MinimaExplorer.exploreFromFile(pslFile, config, queryList)
    if (mapOfArgs.get("--threeValuedLogic").isDefined) {
      for (result <- results) {
        if (result._3 == 0 && result._4 == 0) {
          println(s"${result._1}: false")
        } else if (result._3 == 1 && result._4 == 1) {
          println(s"${result._1}: true")
        } else {
          println(s"${result._1}: unknown")
        }
      }
    } else {
      for (result <- results) {
        println(s"${result._1}: ${result._2} [${result._3}, ${result._4}]")
      }
    }
  }

  System.exit(0)

}
