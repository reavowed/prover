import React from "react";
import {FlexRow} from "./FlexRow";
import {Monospace} from "./Monospace";

export class NavLinks extends React.Component {
  render() {
    const {previous, next} = this.props;
    return <FlexRow className="mt-n2 mb-2">
      {previous && <Monospace.Link href={previous.url}>&laquo; {previous.name}</Monospace.Link>}
      <FlexRow.Grow/>
      {next && <Monospace.Link href={next.url}>{next.name} &raquo;</Monospace.Link>}
    </FlexRow>
  }
}