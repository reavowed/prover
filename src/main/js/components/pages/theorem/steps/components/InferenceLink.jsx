import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import EntryContext from "../../../../EntryContext";
import HashParamsContext from "../../../../HashParamsContext";
import {InferenceSummary} from "../../../../InferenceSummary";

export const InferenceLink = ({inference}) => {
  return <HashParamsContext.Consumer>{params => {
    const shouldHighlight = !inference.isComplete || _.includes(params.inferencesToHighlight, inference.id);
    return <OverlayTrigger placement="bottom"
                           overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
      <a href={inference.url} className="text-uppercase" style={{"fontFamily": "monospace", "color": shouldHighlight ? "red" : "#6c757d"}} tabIndex={-1}>{inference.title}</a>
    </OverlayTrigger>
  }}</HashParamsContext.Consumer> ;
};
