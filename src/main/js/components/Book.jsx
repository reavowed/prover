import React from "react";
import {Breadcrumbs} from "./Breadcrumbs";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";

export class Book extends React.Component {
  render() {
    const {title, url, chapters, previous, next} = this.props;
    return <Page breadcrumbs={<Breadcrumbs links={[{title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}</h3>
      {chapters.map(chapter =>
        <React.Fragment key={chapter.url}>
          <h4 className="mt-3"><a href={chapter.url}>{chapter.title}</a></h4>
          <p>{chapter.summary}</p>
        </React.Fragment>
      )}
    </Page>
  }
}
