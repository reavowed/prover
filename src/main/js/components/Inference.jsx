import React from "react";
import {Breadcrumbs} from "./Breadcrumbs"
import {InferenceSummary} from "./InferenceSummary";
import {Monospace} from "./Monospace";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {Usages} from "./Usages";

export class Inference extends React.Component {
  render() {
    const {inference, title, url, bookLink, chapterLink, previous, next, usages, children, createPremiseElement} = this.props;
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: inference.name, url}]}/>}>
      <NavLinks previous={previous} next={next}/>
      <h3 className="text-center mb-0">{title}: {inference.name}</h3>
      <Monospace className="text-center mb-1">{inference.id}</Monospace>
      <InferenceSummary createPremiseElement={createPremiseElement} inference={inference}/>
      {children}
      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>;
  }
}
