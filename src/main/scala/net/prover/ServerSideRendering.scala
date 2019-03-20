package net.prover

import java.io.InputStreamReader
import java.time.LocalTime

import javax.script.ScriptEngineManager
import jdk.nashorn.api.scripting.NashornScriptEngine

object ServerSideRendering {
  private val engine = {
    val engine = new ScriptEngineManager().getEngineByName("nashorn").asInstanceOf[NashornScriptEngine]
    engine.eval("var window = {};")
    engine.eval(new InputStreamReader(getClass.getClassLoader.getResourceAsStream("static/js/bundle.js")))
    engine
  }

  def render(methodName: String, arguments: AnyRef*): String = synchronized {
    println(s"Started rendering: ${LocalTime.now().toString}")
    val content = engine.invokeMethod(
      engine.get("window"),
      "renderTheoremServer",
      arguments.map(JsonMapping.toString): _*)
    println(s"Finished rendering: ${LocalTime.now().toString}")
    String.valueOf(content)
  }
}
