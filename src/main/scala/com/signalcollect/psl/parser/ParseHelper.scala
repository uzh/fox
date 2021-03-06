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

import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import scala.annotation.tailrec
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical

/**
 * Helper functions for combination parsing.
 */
abstract class ParseHelper[T] extends RegexParsers {

  type Tokens = StdLexical
  val lexical = new StdLexical

  def defaultParser: Parser[T]

  def parse(files: List[File]): T

  def parse(f: File): T = {
    parseFile(f, defaultParser)
  }

  def parse(s: String): T = {
    parseString(s, defaultParser)
  }

  def integer: Parser[Int] = "\\-?[0-9]+".r ^^ (_.toInt)

  def double: Parser[Double] = "\\-?[0-9]+\\.?[0-9]*((e|E)(\\+|-)?[0-9]+)?".r ^^ (_.toDouble)

  def identifier: Parser[String] = "[-a-zA-Z0-9]+".r

  // Classes can be anything or dash.
  def identifierOrDash: Parser[String] = "[-a-zA-Z0-9_]*".r

  //def identifierOrDashOrBracket: Parser[String] = "[-a-zA-Z0-9_\\[\\]\\{\\}]*".r

  // Variables or individuals of sets of vars/individuals.
  def identifierOrBracket: Parser[String] = "[-a-zA-Z0-9\\{\\}\\[\\]]*".r

  // Urls inside quotes.
  def regexUrl: Parser[String] = """<http://[A-Za-z0-9-_]+.[A-Za-z0-9-_:%&?/.=]+>""".r

  /**
   * Helper function for better error messages.
   */
  def nextElements(n: Int, r: Reader[_]): List[String] = {
    @tailrec def nextElementsReversed(elementsSoFar: List[String], n: Int, r: Reader[_]): List[String] = {
      if (n > 0 && !r.atEnd) {
        nextElementsReversed(r.first.toString :: elementsSoFar, n - 1, r.rest)
      } else {
        elementsSoFar
      }
    }
    nextElementsReversed(List(), n, r).reverse
  }

  def parseWith[T](i: Reader[Char], p: Parser[T]): T = {
    val phrased = phrase(p)(i)
    val result: T = phrased match {
      case Success(r, n) => r
      case NoSuccess(msg, reader) => throw new Exception(msg + "\nNext input is " + nextElements(100, reader).mkString("", "", ""))
    }
    result
  }

  def parseFile[T](f: File, p: Parser[T]): T = {
    val input = StreamReader(new BufferedReader(new FileReader(f)))
    parseWith(input, p)
  }

  def parseString[T](s: String, p: Parser[T]): T = {
    val input = new CharArrayReader(s.toArray)
    parseWith(input, p)
  }
}