import React from 'react';
import ReactDOM from 'react-dom';
import {Theorem} from "./components/Theorem";
import {Parser} from "./Parser"

export { Parser };
export function renderTheorem(props, element) {
  ReactDOM.render(<Theorem {...props}/>, element);
}
