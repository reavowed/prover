import React from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import HashParamsContext from "../../../../HashParamsContext";
import {formatHtml} from "../../../../helpers/Formatter";
import {InferenceSummary} from "../../../../InferenceSummary";

export const InferenceLink = ({inference}) => {
  return <HashParamsContext.Consumer>{params => {
    const shouldHighlight = _.includes(params.inferencesToHighlight, inference.id);
    const shouldStrikethrough = !inference.isComplete;
    return <OverlayTrigger placement="bottom"
                           overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
      <a href={inference.url} className="text-uppercase" style={{"fontFamily": "monospace", "color": shouldHighlight ? "red" : "#6c757d", "textDecoration": shouldStrikethrough && "line-through"}} tabIndex={-1}>{formatHtml(inference.title)}</a>
    </OverlayTrigger>
  }}</HashParamsContext.Consumer> ;
};
