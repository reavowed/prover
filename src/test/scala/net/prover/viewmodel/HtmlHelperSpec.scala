package net.prover.viewmodel

import net.prover.model.HtmlHelper
import org.specs2.mutable.Specification

class HtmlHelperSpec extends Specification {
  "HtmlHelper" should {
    "convert subscripts and superscripts into HTML" in {
      HtmlHelper.format("a_b c^d").toString mustEqual "<span>a<sub>b</sub> c<sup>d</sup></span>"
    }
  }
}
