package net.prover.model

import java.util.regex.Pattern

import net.prover.model.proof.InferenceApplication
import net.prover.model.proof.Reference.Elided

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq, Text}

object HtmlHelper {
  def format(text: String): Elem = {
    val matcher = Pattern.compile("(_|\\^)([^\\s)}]+)").matcher(text)
    var indexOfLastMatchEnd = 0
    val childElems = new ListBuffer[Node]
    while (matcher.find()) {
      val intermediateNode = new Text(text.substring(indexOfLastMatchEnd, matcher.start()))
      val innerText = matcher.group(2)
      childElems += intermediateNode
      childElems += (matcher.group(1) match {
        case "_" => <sub>{innerText}</sub>
        case "^" => <sup>{innerText}</sup>
      })
      indexOfLastMatchEnd = matcher.end()
    }
    childElems += new Text(text.substring(indexOfLastMatchEnd))
    <span>{childElems.toList}</span>
  }

  def formatWithReplacement(text: String, replacementFunction: String => Elem): Elem = {
    val matcher = Pattern.compile("(_|\\^)([^\\s)}]+)").matcher(text)
    var indexOfLastMatchEnd = 0
    val childElems = new ListBuffer[Node]
    while (matcher.find()) {
      val intermediateNode = replacementFunction(text.substring(indexOfLastMatchEnd, matcher.start()))
      val innerNode = replacementFunction(matcher.group(2))
      childElems += intermediateNode
      childElems += (matcher.group(1) match {
        case "_" => <sub>{innerNode}</sub>
        case "^" => <sup>{innerNode}</sup>
      })
      indexOfLastMatchEnd = matcher.end()
    }
    childElems += replacementFunction(text.substring(indexOfLastMatchEnd))
    <span>{childElems.toList}</span>
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
