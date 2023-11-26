import React from "react";
import {DefinedExpression} from "../../models/Expression";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {CopiableExpression} from "../expressions/ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {ResultWithPremises} from "../ResultWithPremises";
import {useMappedState} from "./utils/entryFunctions";

export function TermDefinition(props) {
  const {definition: definitionJson, ...otherProps} = props;
  const availableEntries = createAvailableEntries(props);
  const [definition, setDefinition] = useMappedState(definitionJson, availableEntries.parser.parseTermDefinition);

  const equality = _.find(availableEntries.definitions, d => _.includes(d.attributes, "equality"));
  const result = (equality && definition.definingStatement instanceof DefinedExpression && definition.definingStatement.definition === equality && definition.definingStatement.components[0].serialize() === definition.defaultValue.serialize()) ?
    <><CopiableExpression expression={definition.defaultValue}/> is defined to be equal to <CopiableExpression expression={definition.definingStatement.components[1]}/></> :
    <><CopiableExpression expression={definition.defaultValue}/> is defined such that <CopiableExpression expression={definition.definingStatement} splitConjunction /></>;

  return <ExpressionDefinition title="Term Definition" definition={definition} setDefinition={setDefinition} availableEntries={availableEntries} hasDisambiguator {...otherProps}>
    <ResultWithPremises premises={definition.premises}
                        result={result}/>
  </ExpressionDefinition>;
}
