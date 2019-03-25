package net.prover.viewmodel

import net.prover.model.HtmlHelper
import org.specs2.mutable.Specification

import scala.xml.Text

class HtmlHelperSpec extends Specification {
  "HtmlHelper" should {
    "convert subscripts and superscripts into HTML" in {
      HtmlHelper.format("a_b c^d") mustEqual Seq(Text("a"), <sub>b</sub>, Text(" c"), <sup>d</sup>, Text(""))
    }
  }
}
