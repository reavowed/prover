import * as path from "path";
import React from "react";
import {Breadcrumbs} from "./Breadcrumbs";
import {InferenceSummary} from "./InferenceSummary";
import {Monospace} from "./Monospace";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {InlineTextEditor} from "./helpers/InlineTextEditor";
import {Usages} from "./Usages";

export class Inference extends React.Component {
  updateName = (newName) => {
    return window.fetch(path.join(this.props.url, "name"), {method: "PUT", body: newName})
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      })
      .then(url => window.location.pathname = url);
  }
  render() {
    const {inference, title, url, bookLink, chapterLink, previous, next, usages, children, createPremiseElement} = this.props;
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: inference.name, url}]}/>}>
      <NavLinks previous={previous} next={next}/>
      <h3 className="text-center mb-0">
        {title}: <InlineTextEditor text={inference.name} callback={this.updateName}/>
      </h3>
      <Monospace className="text-center mb-1">{inference.id}</Monospace>
      <InferenceSummary createPremiseElement={createPremiseElement} inference={inference}/>
      {children}
      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>;
  }
}
