import React from "react";
import {PremiseReference} from "../models/Step";
import {HighlightableExpression} from "./ExpressionComponent";
import {Inference} from "./Inference";
import {connect, Provider} from 'react-redux';
import { createStore, applyMiddleware } from 'redux';
import Proofs from "./theorem/Proofs";
import TheoremStore, {parseTheorem} from "./theorem/TheoremStore";
import thunkMiddleware from 'redux-thunk'

const Premise = connect(
  (state, {statement, index}) => {
    return {
      expression: statement,
      references: [new PremiseReference(index)],
      boundVariableLists: []
    }
  })(HighlightableExpression);

export class Theorem extends React.Component {
  render() {
    const {theorem: theoremJson, url} = this.props;
    const theorem = parseTheorem(theoremJson);

    const createPremiseElement = (premise, index) => {
      return <Premise statement={premise} index={index}/>
    };

    const store = createStore(TheoremStore, {theorem, url}, applyMiddleware(thunkMiddleware));

    return <Provider store={store}>
      <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
        <Proofs />
      </Inference>
    </Provider> ;
  }
}
