import React from 'react';
import ReactDOM from 'react-dom';
import {Book} from "./components/Book";
import {Books} from "./components/Books";
import {Chapter} from "./components/Chapter";
import {Theorem} from "./components/Theorem"
import {Axiom} from "./components/Axiom";

export { Books, Book, Chapter, Theorem, Axiom };
export function render(component, props) {
  const element = document.createElement("div");
  document.body.append(element);
  ReactDOM.render(React.createElement(component, props), element);
}