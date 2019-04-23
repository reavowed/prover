import React from "react";
import Button from "react-bootstrap/Button";
import {ProofLine} from "./ProofLine";
import {ExpressionComponent} from "../ExpressionComponent";

export class TargetStep extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      boundVariableModalCallback: null,
      findInferenceModalCallbacks: null,
      showFindInferenceModal: false,
      boundVariableName: ""
    };
  }

  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let {proving} = this.state;
    return <>
      <ProofLine.SingleStatementWithPrefix incomplete
                                           editableBoundVariable
                                           prefix="Then"
                                           statement={step.statement}
                                           path={path}
                                           boundVariableLists={boundVariableLists}
                                           additionalReferences={additionalReferences}
                                           buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={() => this.setState({proving: !proving})}>{proving ? "Cancel" : "Prove"}</Button>}
                                           apiService={apiService}
                                           highlighting={highlighting}/>
      {proving && <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}}>
        <h5 className="text-center"><ExpressionComponent expression={step.statement} boundVariableLists={boundVariableLists}/></h5>
      </div>}
    </>
  }
}
