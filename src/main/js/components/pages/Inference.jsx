import * as path from "path";
import React, {useContext, useState} from "react";
import Button from "react-bootstrap/Button";
import DisplayContext from "../DisplayContext";
import ErrorAlert from "../helpers/ErrorAlert";
import InputWithShorthandReplacement from "../helpers/InputWithShorthandReplacement";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {InferenceSummary} from "../InferenceSummary";
import {Monospace} from "../Monospace";
import EditableExplicitName from "./components/EditableExplicitName";
import EditableProperty from "./components/EditableProperty";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import {Usages} from "./components/Usages";
import BoundVariableLists from "./theorem/steps/BoundVariableLists";
import {serializeVariable} from "./utils/entryFunctions";

export function Inference({inference, title, url, bookLink, chapterLink, previous, next, usages, children, buttons, createPremiseElement, editable}) {
  const displayContext = useContext(DisplayContext);
  const boundVariableLists = useContext(BoundVariableLists) || [];
  const [editing, setEditing] = useState(false);
  const [error, setError] = useState(null);
  const updateName = async (newName) => {
    const {url: newUrl} = await window.fetchJson(path.join(url, "name"), {method: "PUT", body: newName});
    window.location.pathname = newUrl;
  };
  const updatePremises = async (newPremises) => {
    await window.fetchJson(path.join(url, "premises"), {method: "PUT", body: _.filter(newPremises.split(/\r?\n/), s => s.length)});
    window.location.reload();
  };
  const updateConclusion = async (newConclusion) => {
    await window.fetchJson(path.join(url, "conclusion"), {method: "PUT", body: newConclusion});
    window.location.reload();
  };
  const updateVariables = async (newVariables) => {
    await window.fetchJson(path.join(url, "variables"), {method: "PUT", body: newVariables});
    window.location.reload();
  };
  function serializeVariables(prefix, variables) {
    if (variables.length > 0) {
      return prefix + " (" + variables.map(serializeVariable).join(", ") + ")"
    }
  }
  const initialVariablesText = _.filter([serializeVariables("statementVariables", inference.variableDefinitions.statements), serializeVariables("termVariables", inference.variableDefinitions.terms)]).join("\n");

  return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: inference.name, url}]}/>}>
    <NavLinks previous={previous} next={next}/>
    <h3 className="text-center mb-0">
      {title}: <InlineTextEditor text={inference.name} callback={updateName}/>
      {buttons && <span className="float-right">{buttons}</span>}
    </h3>
    <Monospace className="text-center mb-1">{inference.id}</Monospace>
    {
      editing ?
        <>
          <Button variant="primary" size="sm" className="float-right ml-1 mb-1" onClick={() => setEditing(false)}>Cancel</Button>
          <ErrorAlert error={error} setError={setError} />
          <EditableProperty label="Variables" initialValue={initialVariablesText} onSave={updateVariables} onError={setError} inputType={InputWithShorthandReplacement} inputProps={{as: "textarea"}} />
          <EditableProperty label="Premises" initialValue={_.map(inference.premises, p => p.serializeNicely(boundVariableLists, displayContext.variableDefinitions)).join("\n")} onSave={updatePremises} onError={setError} inputType={InputWithShorthandReplacement} inputProps={{as: "textarea"}} />
          <EditableProperty label="Conclusion" initialValue={inference.conclusion.serializeNicely(boundVariableLists, displayContext.variableDefinitions)} onSave={updateConclusion} onError={setError} inputType={InputWithShorthandReplacement} />
        </> :
        <>
          {editable && <Button variant="primary" size="sm" className="float-right ml-1" onClick={() => setEditing(true)}>Edit</Button>}
          <InferenceSummary createPremiseElement={createPremiseElement} inference={inference}/>
        </>
    }
    {children}
    <Usages usages={usages}/>
  </Page>;
}
