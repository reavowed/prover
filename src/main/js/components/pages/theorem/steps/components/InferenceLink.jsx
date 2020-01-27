import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import EntryContext from "../../../../EntryContext";
import {InferenceSummary} from "../../../../InferenceSummary";

export const InferenceLink = ({inference, suffix}) => {
  return <OverlayTrigger placement="right"
                         overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
    <a href={inference.url} className="text-uppercase ml-1" style={{"fontFamily": "monospace", "color": inference.isComplete ? "#6c757d" : "red"}}>{inference.title} {suffix}</a>
  </OverlayTrigger>;
};
