import React from "react";
import {Page} from "./Page";

export class Books extends React.Component {
  render() {
    const {books} = this.props;
    return <Page>
      {books.map(book =>
        <h3 className="mt-3">
          <a href={book.url} key={book.url}>{book.title}</a>
        </h3>
      )}
    </Page>
  }
}
