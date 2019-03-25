import React from "react";
import {Page} from "./Page";

export class Books extends React.Component {
  render() {
    const {bookKeys} = this.props;
    return <Page>
      {bookKeys.map(bookKey =>
        <h3 className="mt-3">
          <a href={bookKey.url}>{bookKey.name}</a>
        </h3>
      )}
    </Page>
  }
}