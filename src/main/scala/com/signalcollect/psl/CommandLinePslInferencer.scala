package com.signalcollect.psl

import java.io.File
import java.io.FileWriter
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.admm.utils.MinimaExplorer
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.model.PSLToLPConverter
import com.signalcollect.psl.model.PSLToCvxConverter

object CommandLinePslInferencer extends App {

  val usage = """
Usage: fox filename [--absEps num] [--relEps num] [--maxIter num] [--tol num]
[--queryList "pred1, pred2"] [--multipleMinima true] [--threeValuedLogic true] 
[--output grounding|lp|cvx|inference] [--outfile outputfilename]
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

  val outputType = mapOfArgs.get("--output")
  val outputFile = mapOfArgs.get("--outfile")
  val doInference = !outputType.isDefined || outputType.get == "inference"

  val config = InferencerConfig(
    lazyThreshold = None,
    removeSymmetricConstraints = false,
    maxIterations = mapOfArgs.get("--maxIter").getOrElse("200000").toInt,
    tolerance = mapOfArgs.get("--tol").getOrElse("0").toDouble,
    absoluteEpsilon = mapOfArgs.get("--absEps").getOrElse("1e-8").toDouble,
    relativeEpsilon = mapOfArgs.get("--relEps").getOrElse("1e-5").toDouble)

  val printableResults = if (doInference && !mapOfArgs.get("--multipleMinima").isDefined) {
    // Normal inference.
    val inferenceResults = Inferencer.runInferenceFromFile(
      pslFile = pslFile,
      config = config)
    inferenceResults.printSelected(queryList)
  } else if (doInference) {
    // Multiple minima inference.
    val results = MinimaExplorer.exploreFromFile(pslFile, config, queryList)
    if (mapOfArgs.get("--threeValuedLogic").isDefined) {
      results.map {
        result =>
          if (result._3 == 0 && result._4 == 0) {
            s"${result._1}: false"
          } else if (result._3 == 1 && result._4 == 1) {
            s"${result._1}: true"
          } else {
            s"${result._1}: unknown"
          }
      }.mkString("\n")
    } else {
      results.map {
        result =>
          s"${result._1}: ${result._2} [${result._3}, ${result._4}]"
      }.mkString("\n")
    }
  } else {
    // No inference.
    outputType match {
      case Some("grounding") =>
        val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(PslParser.parse(pslFile), config)
        val results = groundedRules.map(_.toString) ++
          groundedConstraints.map(_.toString)
        results.mkString("\n")
      case Some("lp") => PSLToLPConverter.toLP(pslFile)
      case Some("cvx") => PSLToCvxConverter.toCvx(pslFile)
      case any => s"[Warning]: unknown parameter $any"
    }

  }

  if (outputFile.isDefined) {
    val writer = new FileWriter(outputFile.get)
    writer.append(printableResults)
    writer.close()
  } else {
    println(printableResults)
  }

  System.exit(0)

}
