import React from "react";
import {Parser} from "../../Parser";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";
import {Usages} from "./components/Usages";

export class StatementDefinition extends React.Component {
  render() {
    const {definition: definitionJson, url, bookLink, chapterLink, previous, next, usages, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations} = this.props;
    const parser = new Parser(definitions, typeDefinitions);
    const definition = parser.parseStatementDefinition(definitionJson);
    const entryContext = {parser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};
    return <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>Statement Definition:  <CopiableExpression expression={definition.defaultValue} /></h3>
        {definition.definingStatement && <><CopiableExpression expression={definition.defaultValue} /> is defined by <CopiableExpression expression={definition.definingStatement} />.</>}
        {definition.definingStatement && usages.length > 0 && <hr/>}
        {usages.length > 0 && <Usages usages={usages}/>}
      </Page>
    </EntryContext.Provider>;
  }
}
