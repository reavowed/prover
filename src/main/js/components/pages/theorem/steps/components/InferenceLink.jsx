import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import EntryContext from "../../../../EntryContext";
import HashParamsContext from "../../../../HashParamsContext";
import {InferenceSummary} from "../../../../InferenceSummary";

export const InferenceLink = ({inference, suffix}) => {
  return <HashParamsContext.Consumer>{params => {
    const shouldHighlight = !inference.isComplete || _.includes(params.inferencesToHighlight, inference.id);
    return <OverlayTrigger placement="right"
                           overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
      <a href={inference.url} className="text-uppercase ml-1" style={{"fontFamily": "monospace", "color": shouldHighlight ? "red" : "#6c757d"}}>{inference.title} {suffix}</a>
    </OverlayTrigger>
  }}</HashParamsContext.Consumer> ;
};
