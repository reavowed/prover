import React from "react";
import {Breadcrumbs} from "./Breadcrumbs";
import {Page} from "./Page";

export class Book extends React.Component {
  render() {
    const {title, bookKey, chapters} = this.props;
    return <Page breadcrumbs={<Breadcrumbs.Book bookKey={bookKey}/>}>
      <h3 class="mb-3">{title}</h3>
      {chapters.map(chapter =>
        <div className="chapter">
          <h4><a href={chapter.chapterKey.url}>{chapter.title}</a></h4>
          <p>{chapter.summary}</p>
        </div>
      )}
    </Page>
  }
}