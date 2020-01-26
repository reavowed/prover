import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {ExpressionComponent} from "../../../../../ExpressionComponent";
import BoundVariableLists from "../../BoundVariableLists";

export default function PremiseChooser({premise, setPremise, availablePremises, entryContext}) {
  return <BoundVariableLists.Consumer>{boundVariableLists =>
    <Form.Group>
      <Form.Label><strong>Choose premise</strong></Form.Label>
      <Form.Control as="select" autoFocus value={premise ? premise.serializedReference : ""} onChange={e => setPremise(_.find(availablePremises, p => p.serializedReference === e.target.value))}>
        <option value="" />
        {availablePremises.map(p =>
          <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
              <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext} />
            )}}/>
        )}
      </Form.Control>
    </Form.Group>
  }</BoundVariableLists.Consumer>
}
