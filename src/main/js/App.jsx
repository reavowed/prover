import React from 'react';
import ReactDOM from 'react-dom';
import {Book} from "./components/pages/Book";
import {Books} from "./components/pages/Books";
import {Chapter} from "./components/pages/Chapter";
import {Theorem} from "./components/pages/Theorem"
import {Axiom} from "./components/pages/Axiom";
import {StatementDefinition} from "./components/pages/StatementDefinition"
import {TermDefinition} from "./components/pages/TermDefinition"
import {TypeDefinition} from "./components/pages/TypeDefinition"
import {TypeQualifierDefinition} from "./components/pages/TypeQualifierDefinition"
import {StandalonePropertyDefinition} from "./components/pages/StandalonePropertyDefinition";
import {PropertyDefinitionOnType} from "./components/pages/PropertyDefinitionOnType";
import {RelatedObjectDefinition} from "./components/pages/RelatedObjectDefinition";
import {TypeRelationDefinition} from "./components/pages/TypeRelationDefinition";

React.Component.prototype.setStatePromise = function(newState) {
  return new Promise(resolve => this.setState(newState, resolve));
};

Window.prototype.fetchJson = function(input, init) {
  const {body, headers, ...otherInit} = init || {};
  return this.fetch(input, {
    ...otherInit,
    body: body && JSON.stringify(body),
    headers: {"Content-Type": "application/json", ...(headers || {})}
  })
    .then(response => new Promise(((resolve, reject) => {
        const method = response.ok ? resolve : reject;
        return response.headers.get("Content-Length") === "0" ? method() : response.json().then(method);
      }))
    )
};

String.prototype.capitalize = function() {
  return this.replace(/(?:^|\s)\S/g, function(a) { return a.toUpperCase(); });
};


export { Books, Book, Chapter, Theorem, Axiom, StatementDefinition, TermDefinition, TypeDefinition, TypeQualifierDefinition, TypeRelationDefinition, StandalonePropertyDefinition, PropertyDefinitionOnType, RelatedObjectDefinition };
export function render(component, props) {
  const element = document.createElement("div");
  element.style.height="100%";
  document.body.append(element);
  ReactDOM.render(React.createElement(component, props), element);
}
