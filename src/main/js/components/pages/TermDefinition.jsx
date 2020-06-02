import React from "react";
import {DefinedExpression} from "../../models/Expression";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {ResultWithPremises} from "../ResultWithPremises";
import {useMappedState} from "./utils/entryFunctions";

export function TermDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseTermDefinition);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  const equality = _.find(entryContext.definitions, d => _.includes(d.attributes, "equality"));
  const result = (equality && definition.definingStatement instanceof DefinedExpression && definition.definingStatement.definition === equality && definition.definingStatement.components[0].serialize() === definition.defaultValue.serialize()) ?
    <><CopiableExpression expression={definition.defaultValue}/> is defined to be equal to <CopiableExpression expression={definition.definingStatement.components[1]}/></> :
    <><CopiableExpression expression={definition.defaultValue}/> is defined such that <CopiableExpression expression={definition.definingStatement} splitConjunction /></>;

  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, entryContext)}>
    <ExpressionDefinition title="Term Definition" definition={definition} setDefinition={setDefinition} entryContext={entryContext} parser={parser} hasDisambiguator {...otherProps}>
      <ResultWithPremises premises={definition.premises}
                          result={result}/>
    </ExpressionDefinition>
  </DisplayContext.Provider>;
}
