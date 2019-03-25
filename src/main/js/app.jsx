import React from 'react';
import ReactDOM from 'react-dom';
import {Books} from "./components/Books";
import {Theorem} from "./components/Theorem";

export { Theorem, Books };
export function render(component, props) {
  const element = document.createElement("div");
  document.body.append(element);
  ReactDOM.render(React.createElement(component, props), element);
}
