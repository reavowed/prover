import React from "react";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {ExpressionComponent} from "./ExpressionComponent";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {Usages} from "./Usages";

export class StatementDefinition extends React.Component {
  render() {
    const {definition: definitionJson, url, bookLink, chapterLink, previous, next, usages} = this.props;
    const definition = Parser.parseStatementDefinition(definitionJson);
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>Statement Definition:  <ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/></h3>
      {definition.definingStatement && <><ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/> is defined by <ExpressionComponent expression={definition.definingStatement} boundVariableLists={[]}/>.</>}
      {definition.definingStatement && usages.length > 0 && <hr/>}
      {usages.length > 0 && <Usages usages={usages}/>}
    </Page>;
  }
}
