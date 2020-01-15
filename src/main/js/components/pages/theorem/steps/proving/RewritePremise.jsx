import _ from "lodash";
import React, {useContext, useState} from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {ExpressionComponent} from "../../../../ExpressionComponent";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import Rewriter from "./components/Rewriter";

export default function RewritePremise({path, availablePremises, entryContext, onCancel, onError}) {
  const context = useContext(ProofContext);
  const [premiseToRewrite, setPremiseToRewrite] = useState(null);

  const rewrite = (rewrites) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "rewritePremise", {method: "POST", rewrites})
      .then(onCancel)
      .catch(onError);
  };
  return <BoundVariableLists.Consumer>{boundVariableLists =>
    <>
      <Form.Group>
        <Form.Label><strong>Choose premise</strong></Form.Label>
        <Form.Control as="select" autoFocus value={premiseToRewrite && premiseToRewrite.serializedReference} onChange={e => setPremiseToRewrite(_.find(availablePremises, p => p.serializedReference === e.target.value).statement)}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
      {premiseToRewrite && <Rewriter title="Rewriting Premise"
                                     expression={premiseToRewrite}
                                     path={path}
                                     onSave={rewrites => rewrite({serializedPremise: premiseToRewrite.serialize(), rewrites})}
      />}
    </>
  }</BoundVariableLists.Consumer>;
}
