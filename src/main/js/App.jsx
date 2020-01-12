import React from 'react';
import ReactDOM from 'react-dom';
import {Book} from "./components/pages/Book";
import {Books} from "./components/pages/Books";
import {Chapter} from "./components/pages/Chapter";
import {Theorem} from "./components/pages/Theorem"
import {Axiom} from "./components/pages/Axiom";
import {StatementDefinition} from "./components/pages/StatementDefinition"
import {TermDefinition} from "./components/pages/TermDefinition"

export { Books, Book, Chapter, Theorem, Axiom, StatementDefinition, TermDefinition };
export function render(component, props) {
  const element = document.createElement("div");
  element.style.height="100%";
  document.body.append(element);
  ReactDOM.render(React.createElement(component, props), element);
}
