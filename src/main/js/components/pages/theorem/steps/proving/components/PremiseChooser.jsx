import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {DisplaySettingsContext} from "../../../../../DisplaySettings";
import BoundVariableListContext from "../../../../../expressions/boundVariables/BoundVariableListContext";
import {ExpressionComponent} from "../../../../../expressions/ExpressionComponent";

export default function PremiseChooser({premise, setPremise, availablePremises, availableEntries, title, autoFocus}) {
  return <BoundVariableListContext.Consumer>{boundVariableLists =>
    <DisplaySettingsContext.Consumer>{displaySettings =>
      <Form.Group>
        <Form.Label><strong>{title || "Choose premise"}</strong></Form.Label>
        <Form.Control as="select" autoFocus={autoFocus} value={premise ? premise.statement.serialize() : ""} onChange={e => setPremise(_.find(availablePremises, p => p.statement.serialize() === e.target.value))}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.statement.serialize()} value={p.statement.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} availableEntries={availableEntries} displaySettings={displaySettings} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
    }</DisplaySettingsContext.Consumer>
  }</BoundVariableListContext.Consumer>
}
