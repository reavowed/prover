import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import {ExpressionComponent} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";

export class TargetStep extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      proving: false
    };
  }
  introduceBoundVariable = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "introduceBoundVariable", {
      method: "POST"
    }).then(this.props.theoremContext.updateTheorem);
  };
  introduceDeduction = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "introduceDeduction", {
      method: "POST"
    }).then(this.props.theoremContext.updateTheorem);
  };
  extract = () => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "extract", { method: "POST" })
      .then(this.props.theoremContext.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, theoremContext, boundVariableLists} = this.props;
    let {proving} = this.state;
    let scopingStatement = _.find(window.definitions, d => _.includes(d.attributes, "scoping"));
    let deductionStatement = _.find(window.definitions, d => _.includes(d.attributes, "deduction"));
    return <>
      <ProofLine.SingleStatementWithPrefix incomplete
                                           editableBoundVariable
                                           prefix="Then"
                                           statement={step.statement}
                                           path={path}
                                           boundVariableLists={boundVariableLists}
                                           additionalReferences={additionalReferences}
                                           buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={() => this.setState({proving: !proving})}>{proving ? "Cancel" : "Prove"}</Button>}
                                           theoremContext={theoremContext} />
      {proving && <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}}>
        <h5 className="text-center"><ExpressionComponent expression={step.statement} boundVariableLists={boundVariableLists}/></h5>
        <div className="text-center">
          <Button size="sm" onClick={this.extract}>Extract</Button>
          {scopingStatement && step.statement.definition === scopingStatement &&
          <Button size="sm" className="ml-1" onClick={this.introduceBoundVariable}>Introduce bound variable</Button>}
          {deductionStatement && step.statement.definition === deductionStatement &&
          <Button size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
        </div>
      </div>}
    </>
  }
}
