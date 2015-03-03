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

package com.signalcollect.psl.parser

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader
import com.signalcollect.psl.model.DistanceMeasure
import com.signalcollect.psl.model.Predicate
import com.signalcollect.psl.model.PredicateInRule
import com.signalcollect.psl.model.PredicateProperty
import com.signalcollect.psl.model.Rule
import java.io.File
import scala.util.parsing.input.StreamReader
import java.io.BufferedReader
import java.io.FileReader
import scala.util.parsing.input.Reader
import scala.annotation.tailrec

case class EncodedVariable(id: Int, binding: Double)

/**
 * A parser of text files for results in the format:
 * -1000: 0.4
 * -2: 0
 * -3: 1
 */
object PslResultParser extends ParseHelper[List[EncodedVariable]] with ImplicitConversions {

  protected override val whiteSpace = """(\s|//.*|#.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def defaultParser = variables
  
  def parse(files: List[File]): List[EncodedVariable]= {
    val parsedFiles = files.map(parseFile(_, defaultParser))
    parsedFiles.foldLeft(List.empty[EncodedVariable]) (_ ++ _)
  }

   lazy val variable: Parser[EncodedVariable] = {
    integer ~! ":" ~! double ^^ {
      case gpId ~ ":" ~ binding =>
        EncodedVariable(gpId, binding)
    }
  }
 
  lazy val variables: Parser[List[EncodedVariable]] = {
    rep(variable) ^^ {
      case variables => variables
    }
  }
}
