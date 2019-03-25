import React from "react";
import {Breadcrumbs} from "./Breadcrumbs";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";

export class Book extends React.Component {
  render() {
    const {title, bookKey, chapters, previous, next} = this.props;
    return <Page breadcrumbs={<Breadcrumbs.Book bookKey={bookKey}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}</h3>
      {chapters.map(chapter =>
        <React.Fragment key={chapter.chapterKey.value}>
          <h4 className="mt-3"><a href={chapter.chapterKey.url}>{chapter.title}</a></h4>
          <p>{chapter.summary}</p>
        </React.Fragment>
      )}
    </Page>
  }
}
