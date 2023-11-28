import React, {useContext} from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import {formatHtml} from "../../../../helpers/Formatter";
import {InferenceSummary} from "../../../../InferenceSummary";
import TheoremContext from "../../TheoremContext";

export const InferenceLink = ({inference}) => {
  const theoremContext = useContext(TheoremContext);
  const shouldHighlight = _.includes(theoremContext.inferencesToHighlight, inference.id);
  const shouldStrikethrough = !inference.isComplete;
  return <OverlayTrigger placement="bottom"
                         overlay={<Popover title={inference.name}><InferenceSummary inference={inference}/></Popover>}>
    <a href={inference.url} className="text-uppercase" style={{"fontFamily": "monospace", "color": shouldHighlight ? "red" : "#6c757d", "textDecoration": shouldStrikethrough && "line-through"}} tabIndex={-1}>{formatHtml(inference.title)}</a>
  </OverlayTrigger>;
};
