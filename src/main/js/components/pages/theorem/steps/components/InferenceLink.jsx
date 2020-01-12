import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import EntryContext from "../../../../EntryContext";
import {InferenceSummary} from "../../../../InferenceSummary";

export const InferenceLink = ({inference, suffix}) => {
  const entryContext = useContext(EntryContext);
  const linkSummary = entryContext.inferences[inference.id];
  return <OverlayTrigger placement="right"
                         overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
    <a href={linkSummary.url} className="text-muted text-uppercase ml-1" style={{"fontFamily": "monospace"}}>{linkSummary.title} {suffix}</a>
  </OverlayTrigger>;
};
