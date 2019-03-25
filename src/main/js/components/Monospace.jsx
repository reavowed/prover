import styled from "styled-components";

function wrapElement(element) {
  return styled(element)`
    font-family: monospace;
    color: gray !important;
  `;
}

export const Monospace = wrapElement("div");
Monospace.Link = wrapElement("a");
Monospace.Text = wrapElement("span");