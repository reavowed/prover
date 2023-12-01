import styled from "styled-components";

function wrapElement(element) {
  return styled(element)`
    font-family: monospace;
    color: gray !important;
  `;
}

const Monospace = wrapElement("div");
const Link = wrapElement("a");
const Text = wrapElement("span");

export default Object.assign(Monospace, {
  Link,
  Text
})
