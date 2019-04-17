package net.prover.controllers

import net.prover.JsonMapping

import scala.xml.Unparsed

trait ReactViews {
  protected def createReactView(viewName: String, props: AnyRef, globals: Map[String, AnyRef] = Map.empty): String = {
    val initScript = (globals.map { case (name, value) => s"window.$name = ${JsonMapping.toString(value)};" }.toSeq
      :+ s"App.render(App.$viewName, ${JsonMapping.toString(props)});"
      ).mkString("\n")
    <html>
      <head>
        <title>Prover</title>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous" />
        <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.8.0/css/all.css" integrity="sha384-Mmxa0mLqhmOeaE8vgOSbKacftZcsNYDjQzuCOm6D02luYSzBG8vpaOykv9lFQ51Y" crossorigin="anonymous" />
        <style>{"body { height: 100%; } .popover { max-width: 100%; }"}</style>
      </head>
      <body>
        <script src="http://localhost:8079/node_modules/react/umd/react.development.js"></script>
        <script src="http://localhost:8079/node_modules/react-dom/umd/react-dom.development.js"></script>
        <script src="http://localhost:8079/bundle.js"></script>
        <script type="text/javascript">{Unparsed(initScript)}</script>
      </body>
    </html>.toString()
  }
}
