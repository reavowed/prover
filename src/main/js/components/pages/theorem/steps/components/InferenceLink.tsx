import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import {formatHtml} from "../../../../helpers/Formatter";
import {InferenceSummary} from "../../../../InferenceSummary";
import TheoremContext from "../../TheoremContext";
import {InferenceWithSummary} from "../../../../definitions/EntryDefinitions";
import _ from "lodash";

export const InferenceLink = ({inference}: {inference: InferenceWithSummary}) => {
  const theoremContext = useContext(TheoremContext)!;
  const shouldHighlight = _.includes(theoremContext.inferencesToHighlight, inference.id);
  const shouldStrikethrough = !inference.isComplete;
  const popover = <Popover id="inferenceSummary">
     <Popover.Title>{inference.name}</Popover.Title>
     <Popover.Content>
      <InferenceSummary inference={inference}/>
     </Popover.Content>
  </Popover>
  return <OverlayTrigger placement="bottom"
                         overlay={popover}>
    <a href={inference.url} className="text-uppercase" style={{"fontFamily": "monospace", "color": shouldHighlight ? "red" : "#6c757d", "textDecoration": shouldStrikethrough ? "line-through" : undefined}} tabIndex={-1}>{formatHtml(inference.title)}</a>
  </OverlayTrigger>;
};
