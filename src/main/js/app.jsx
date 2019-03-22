import React from 'react';
import ReactDOM from 'react-dom';
import {Theorem} from "./components/Theorem";
import {Parser} from "./Parser"

class TheoremPage extends React.Component {
  render() {
    const {theorem: rawTheorem, ...otherProps} = this.props;
    const theorem = Parser.parseTheorem(rawTheorem);
    return <Theorem theorem={theorem} {...otherProps} />
  }
}

ReactDOM.render(<TheoremPage theorem={theorem} previousEntry={previousEntry} nextEntry={nextEntry} usages={usages}/>, document.getElementById("theorem"));
