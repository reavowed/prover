import React from "react";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {ExpressionComponent} from "./ExpressionComponent";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {ResultWithPremises} from "./ResultWithPremises";
import {Usages} from "./Usages";

export class TermDefinition extends React.Component {
  render() {
    const {definition: definitionJson, url, bookLink, chapterLink, previous, next, usages} = this.props;
    const definition = Parser.parseTermDefinition(definitionJson);
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>Term Definition:  <ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/></h3>
      <ResultWithPremises premises={definition.premises}
                          result={<><ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/> is defined by <ExpressionComponent expression={definition.definingStatement} boundVariableLists={[]}/></>}/>
      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>;
  }
}
