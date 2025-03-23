package net.prover.controllers

import net.prover.JsonMapping
import net.prover.controllers.models.{DefinitionSummary, StandalonePropertyDefinitionSummary, TypeDefinitionSummary, TypeRelationDefinitionSummary}
import net.prover.model.*
import net.prover.model.entries.DisplayShorthand

import scala.xml.Unparsed

trait ReactViews {
  protected def createReactView(viewName: String, props: AnyRef): String = {
    "<!doctype html>" +
    <html>
      <head>
        <title>Prover</title>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous" />
        <link rel="stylesheet" href="https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css" />
        <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.8.0/css/all.css" integrity="sha384-Mmxa0mLqhmOeaE8vgOSbKacftZcsNYDjQzuCOm6D02luYSzBG8vpaOykv9lFQ51Y" crossorigin="anonymous" />
        <style>{"html, body { height: 100%; } .popover { max-width: 100%; }"}</style>
      </head>
      <body>
        <script crossorigin=""  src="http://localhost:8079/bundle.js"></script>
        <script type="text/javascript">{Unparsed(s"App.render(App.$viewName, ${JsonMapping.toString(props)});")}</script>
      </body>
    </html>.toString()
  }

  protected def getGeneralDisplayProps(availableEntries: AvailableEntries): Map[String, AnyRef] = {
    Map(
      "definitions" -> DefinitionSummary.getAllFromContext(availableEntries),
      "typeDefinitions" -> TypeDefinitionSummary.getAllFromContext(availableEntries),
      "standalonePropertyDefinitions" -> StandalonePropertyDefinitionSummary.getAllFromContext(availableEntries),
      "typeRelationDefinitions" -> TypeRelationDefinitionSummary.getAllFromContext(availableEntries),
      "displayShorthands" -> availableEntries.allEntries.ofType[DisplayShorthand],
      "definitionShorthands" -> DefinitionSummary.getDefinitionShorthandsFromContext(availableEntries))
  }
}
