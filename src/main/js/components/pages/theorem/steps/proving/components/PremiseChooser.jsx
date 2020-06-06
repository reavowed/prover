import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import DisplayContext from "../../../../../DisplayContext";
import {ExpressionComponent} from "../../../../../ExpressionComponent";
import BoundVariableLists from "../../BoundVariableLists";

export default function PremiseChooser({premise, setPremise, availablePremises, entryContext, title, autoFocus}) {
  return <BoundVariableLists.Consumer>{boundVariableLists =>
    <DisplayContext.Consumer>{displayContext =>
      <Form.Group>
        <Form.Label><strong>{title || "Choose premise"}</strong></Form.Label>
        <Form.Control as="select" autoFocus={autoFocus} value={premise ? premise.statement.serialize() : ""} onChange={e => setPremise(_.find(availablePremises, p => p.statement.serialize() === e.target.value))}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.statement.serialize()} value={p.statement.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext} displayContext={displayContext} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
    }</DisplayContext.Consumer>
  }</BoundVariableLists.Consumer>
}
