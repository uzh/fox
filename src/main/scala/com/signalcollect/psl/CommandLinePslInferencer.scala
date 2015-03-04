package com.signalcollect.psl

import java.io.File
import java.io.FileWriter
import java.io.FileReader
import com.signalcollect.psl.model.GroundedPredicate
import com.signalcollect.admm.utils.MinimaExplorer
import com.signalcollect.psl.parser.PslParser
import com.signalcollect.psl.model.PSLToLPConverter
import com.signalcollect.psl.model.LpResultParser
import com.signalcollect.psl.model.PSLToCvxConverter

import scala.sys.process._

object CommandLinePslInferencer extends App {

  val usage = """
Usage: fox filename [--absEps num] [--relEps num] [--maxIter num] [--tol num]
[--queryList "pred1, pred2"] [--multipleMinima true] [--threeValuedLogic true] 
[--output grounding|ilp|lp|cvx|inference] [--outfile outputfilename]
[--inference foxPSL|mosekLP|mosekILP]
[--breezeOptimizer true|false]

--absEps, --relEps: absolute and relative epsilons for ADMM algorithm (foxPSL solver)
--maxIter: maximum number of iterations for ADMM algorithm (foxPSL solver)
--tol: tolerance value for constraints, under this threshold a constraint is not considered broken
--queryList: list of predicate names that we want to output
--multipleMinima: experimental feature that finds the range of the best truth values for each predicates
--threeValuedLogic: outputs true, false or unknown if multipleMinima is on
--output: what type of output do we expect: grounding (grounded rules), a file/string in LP format (input to ILP solvers), a file/string in CVX format (input to CVX toolbox in Matlab), standard inference results
--outfile: if defined the output is saved in this file, otherwise it is shown in the stdout
--inference: which solver to use for inference, foxPSL or mosek (version LP and ILP) - requires mosek to be installed, and currently works only for problems with hard rules and linear soft rules with one clause.
--breezeOptimizer: if we use foxPSL, we can choose whether to use the Breeze toolkit to do the single minimizations.
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
  val inference = mapOfArgs.getOrElse("--inference", "foxPSL")
  val doFoxPSLInference = doInference && inference == "foxPSL"
  val doMosekLPInference = doInference && inference == "mosekLP"
  val doMosekILPInference = doInference && (inference == "mosekILP" || inference == "mosek")

  val config = InferencerConfig(
    lazyThreshold = None,
    removeSymmetricConstraints = false,
    maxIterations = mapOfArgs.get("--maxIter").getOrElse("200000").toInt,
    tolerance = mapOfArgs.get("--tol").getOrElse("0").toDouble,
    absoluteEpsilon = mapOfArgs.get("--absEps").getOrElse("1e-8").toDouble,
    relativeEpsilon = mapOfArgs.get("--relEps").getOrElse("1e-5").toDouble,
    breezeOptimizer = mapOfArgs.get("--breezeOptimizer").getOrElse("false").toBoolean)

  val (printableResults, extraInformation) = if (doFoxPSLInference && !mapOfArgs.get("--multipleMinima").isDefined) {
    // Normal inference.
    val inferenceResults = Inferencer.runInferenceFromFile(
      pslFile = pslFile,
      config = config)
    (inferenceResults.printSelected(queryList), None)
  } else if (doFoxPSLInference) {
    // Multiple minima inference.
    val results = MinimaExplorer.exploreFromFile(pslFile, config, queryList)
    val stringOfResults = if (mapOfArgs.get("--threeValuedLogic").isDefined) {
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
    (stringOfResults, None)
  } else if (doMosekILPInference) {
    val (translatedProblem, idToGpMap) = PSLToLPConverter.toLP(pslFile, isBinary = true)
    // Write translation to file.
    val writer = new FileWriter("temp-mosek-translation.lp")
    writer.append(translatedProblem)
    writer.close()
    // Execute Mosek.
    val mosekCommand = "mosek temp-mosek-translation.lp"
    val mosekOutput = mosekCommand.!!
    //println(mosekOutput)
    // Read the output.
    val mosekResult = LpResultParser.parse(new File("temp-mosek-translation.int"))
    val mergedResults = mosekResult.map { case (id, value) => (idToGpMap(id), value) }.mkString("\n")
    (mergedResults, None)
  } else if (doMosekLPInference) {
    val (translatedProblem, idToGpMap) = PSLToLPConverter.toLP(pslFile, isBinary = false)
    // Write translation to file.
    val writer = new FileWriter("temp-mosek-translation.lp")
    writer.append(translatedProblem)
    writer.close()
    // Execute Mosek.
    val mosekCommand = "mosek temp-mosek-translation.lp"
    val mosekOutput = mosekCommand.!!
    //println(mosekOutput)
    // Read the output.
    val mosekResult = LpResultParser.parse(new File("temp-mosek-translation.sol"))
    val mergedResults = mosekResult.map { case (id, value) => (idToGpMap(id), value) }.mkString("\n")
    (mergedResults, None)
  } else {
    // No inference.
    outputType match {
      case Some("grounding") =>
        val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(PslParser.parse(pslFile), config)
        val results = groundedRules.map(_.toString) ++
          groundedConstraints.map(_.toString)
        (results.mkString("\n"), None)
      case Some("lp") =>
        val (translatedProblem, idToGpName) = PSLToLPConverter.toLP(pslFile, isBinary = false)
        (translatedProblem, Some(idToGpName))
      case Some("ilp") =>
        val (translatedProblem, idToGpName) = PSLToLPConverter.toLP(pslFile, isBinary = true)
        (translatedProblem, Some(idToGpName))
      case Some("cvx") =>
        val (translatedProblem, idToGpName) = PSLToCvxConverter.toCvx(pslFile)
        (translatedProblem, Some(idToGpName))
      case any =>
        (s"[Warning]: unknown parameter $any", None)
    }
  }

  if (outputFile.isDefined) {
    val writer = new FileWriter(outputFile.get)
    writer.append(printableResults)
    writer.append("\n")
    writer.close()
    if (extraInformation.isDefined) {
      val writerMap = new FileWriter(outputFile.get + ".map")
      writerMap.append(extraInformation.get.mkString("\n"))
      writerMap.append("\n")
      writerMap.close()
    }
  } else {
    println(printableResults)
  }

  System.exit(0)

}
