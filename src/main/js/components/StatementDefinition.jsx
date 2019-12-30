import React from "react";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {CopiableExpression} from "./ExpressionComponent";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {Usages} from "./Usages";

export class StatementDefinition extends React.Component {
  render() {
    const {definition: definitionJson, url, bookLink, chapterLink, previous, next, usages} = this.props;
    const definition = Parser.parseStatementDefinition(definitionJson);
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>Statement Definition:  <CopiableExpression expression={definition.defaultValue} /></h3>
      {definition.definingStatement && <><CopiableExpression expression={definition.defaultValue} /> is defined by <CopiableExpression expression={definition.definingStatement} />.</>}
      {definition.definingStatement && usages.length > 0 && <hr/>}
      {usages.length > 0 && <Usages usages={usages}/>}
    </Page>;
  }
}
