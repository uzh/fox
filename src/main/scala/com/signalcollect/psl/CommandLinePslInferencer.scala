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
import com.signalcollect.psl.model.PSLToMLNConverter
import com.signalcollect.external.parser.CausalDiscoveryAspParser
import com.signalcollect.admm.utils.Timer

object CommandLinePslInferencer extends App {

  val usage = """
Usage: fox filename [--absEps num] [--relEps num] [--maxIter num] [--tol num]
[--queryList "pred1, pred2"] [--multipleMinima true] [--threeValuedLogic true]
[--output grounding|ilp|lp|cvx|mln|inference|shortInference|onlyTrueFacts] [--outfile outputfilename]
[--inference foxPSL|mosekLP|mosekILP]
[--breezeOptimizer true|false]
[--setsFile filename --factsFile filename]

--absEps, --relEps: absolute and relative epsilons for ADMM algorithm (foxPSL solver)
--maxIter: maximum number of iterations for ADMM algorithm (foxPSL solver)
--tol: tolerance value for constraints, under this threshold a constraint is not considered broken
--queryList: list of predicate names that we want to output
--multipleMinima: experimental feature that finds the range of the best truth values for each predicates
--threeValuedLogic: outputs true, false or unknown if multipleMinima is on
--output: what type of output do we expect: grounding (grounded rules), a file/string in LP format (input to ILP solvers), 
a file/string in CVX format (input to CVX toolbox in Matlab), two files of grounded MLN rules and evidences(mln), standard inference results, 
shortInference (inference results without the technical details, e.g. "causes(x,y) = 0.0", 
onlyTrueFacts (only the inferred facts that have a truth value >0.5 and without truth value, separated by a comma).
--outfile: if defined the output is saved in this file, otherwise it is shown in the stdout
--inference: which solver to use for inference, foxPSL or mosek (version LP and ILP) - requires mosek to be installed, and currently works only for problems with hard rules and linear soft rules with one clause.
--breezeOptimizer: if we use foxPSL, we can choose whether to use the Breeze toolkit to do the single minimizations.
--setsFile/--factsFile: used for reading the causal ASP problems, the fileanem containing the sets description and the filename containing the facts file.

Example for causal discovery rules:
./fox.sh examples/causalDiscoveryRules.psl --setsFile causalDiscoveryTmp/pipeline.pre.asp --factsFile causalDiscoveryTmp/pipeline.ind --outputType onlyTrueFacts --queryList causes
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
  val doInference = !outputType.isDefined || outputType.get == "inference" || outputType.get == "shortInference" || outputType.get == "onlyTrueFacts" 
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

  val (pslData, parsingTime) = Timer.time {
    if (config.parallelizeParsing) {
      PslParser.parseFileLineByLine(pslFile).toParsedPslFile()
    } else {
      PslParser.parse(pslFile)
    }
  }

  val updatedPslData = if (mapOfArgs.get("--setsFile").isDefined) {
    val setsFile = new File(mapOfArgs.get("--setsFile").get)
    if (!mapOfArgs.get("--factsFile").isDefined) {
      println("[ERROR]: no facts file specified, no inference performed")
      System.exit(1)
    }
    val factsFile = new File(mapOfArgs.get("--factsFile").get)
    CausalDiscoveryAspParser.updateParsedPslFile(pslData, factsFile, setsFile)
  } else {
    pslData
  }

  val (printableResults, extraInformation) = if (doFoxPSLInference && !mapOfArgs.get("--multipleMinima").isDefined) {
    // Normal inference.
    val inferenceResults = Inferencer.runInference(updatedPslData, parsingTime, None, config = config)
    (inferenceResults.printSelectedResults(queryList, printFacts = true, outputType = outputType.getOrElse("inference")), None)
  } else if (doFoxPSLInference) {
    // Multiple minima inference.
    val results = MinimaExplorer.runExploration(updatedPslData, config, queryList)
    (MinimaExplorer.printSelectedResults(results, mapOfArgs.get("--threeValuedLogic").isDefined, short = (outputType.getOrElse("inference") == "shortInference")), None)
  } else if (doMosekILPInference) {
    val results = PSLToLPConverter.solve(updatedPslData, isBinary = true)
    (PSLToLPConverter.printSelectedResults(results, queryList, outputType = outputType.getOrElse("inference"), printBinary = true), None)
  } else if (doMosekLPInference) {
    val results = PSLToLPConverter.solve(updatedPslData, isBinary = false)
    (PSLToLPConverter.printSelectedResults(results, queryList, outputType = outputType.getOrElse("inference")), None)
  } else {
    // No inference.
    outputType match {
      case Some("grounding") =>
        val (groundedRules, groundedConstraints, idToGpMap) = Grounding.ground(updatedPslData, config)
        val results = groundedRules.map(_.toString) ++
          groundedConstraints.map(_.toString)
        (results.mkString("\n"), None)
      case Some("lp") =>
        val (translatedProblem, idToGpName) = PSLToLPConverter.toLP(updatedPslData, isBinary = false)
        (translatedProblem, Some(idToGpName))
      case Some("ilp") =>
        val (translatedProblem, idToGpName) = PSLToLPConverter.toLP(updatedPslData, isBinary = true)
        (translatedProblem, Some(idToGpName))
      case Some("cvx") =>
        val (translatedProblem, idToGpName) = PSLToCvxConverter.toCvx(updatedPslData)
        (translatedProblem, Some(idToGpName))
      case Some("mln") =>
        val (evidence, mlnrules, idToGpName) = PSLToMLNConverter.toMLN(updatedPslData)
        (evidence + mlnrules, Some(idToGpName))
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
