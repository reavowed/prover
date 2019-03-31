import React from "react";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Popover from "react-bootstrap/Popover";
import {InferenceSummary} from "../InferenceSummary";

export const InferenceLink = ({inference, suffix}) =>
  <OverlayTrigger
    placement="right"
    overlay={<Popover title={inference.name}><InferenceSummary inference={inference} /></Popover>}
  >
    <a href={inference.key.url} className="text-muted text-uppercase ml-1" style={{"fontFamily": "monospace"}}>{inference.name} {suffix}</a>
  </OverlayTrigger>;
