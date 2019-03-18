package net.prover.model

import java.util.regex.Pattern

import net.prover.model.proof.InferenceApplication
import net.prover.model.proof.Reference.Elided

import scala.collection.mutable.ListBuffer
import scala.xml.{Node, Text}

object HtmlHelper {
  def format(text: String): Seq[Node] = {
    val matcher = Pattern.compile("(_|\\^)([^\\s)}]+)").matcher(text)
    var indexOfLastMatchEnd = 0
    val nodes = new ListBuffer[Node]
    while (matcher.find()) {
      val intermediateNode = new Text(text.substring(indexOfLastMatchEnd, matcher.start()))
      val innerText = matcher.group(2)
      nodes += intermediateNode
      nodes += (matcher.group(1) match {
        case "_" => <sub>{innerText}</sub>
        case "^" => <sup>{innerText}</sup>
      })
      indexOfLastMatchEnd = matcher.end()
    }
    nodes += new Text(text.substring(indexOfLastMatchEnd))
    nodes.toList
  }

  def formatWithReplacement(text: String, replacementFunction: String => Seq[Node]): Seq[Node] = {
    val matcher = Pattern.compile("(_|\\^)([^\\s)}]+)").matcher(text)
    var indexOfLastMatchEnd = 0
    val nodes = new ListBuffer[Node]
    while (matcher.find()) {
      val intermediateNodes = replacementFunction(text.substring(indexOfLastMatchEnd, matcher.start()))
      val innerNode = replacementFunction(matcher.group(2))
      nodes ++= intermediateNodes
      nodes += (matcher.group(1) match {
        case "_" => <sub>{innerNode}</sub>
        case "^" => <sup>{innerNode}</sup>
      })
      indexOfLastMatchEnd = matcher.end()
    }
    nodes ++= replacementFunction(text.substring(indexOfLastMatchEnd))
    nodes.toList
  }

  def findInferenceToDisplay(inferenceApplication: InferenceApplication): Inference = {
    inferenceApplication.references.collect {
      case Elided(innerApplication) => innerApplication
    } match {
      case Seq(innerApplication) => findInferenceToDisplay(innerApplication)
      case Nil => inferenceApplication.inference
    }
  }
}
