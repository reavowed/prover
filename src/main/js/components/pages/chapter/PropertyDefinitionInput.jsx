import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import TypeAutocomplete from "../components/TypeAutocomplete";
import TypeObjectsAutocomplete from "../components/TypeObjectsAutocomplete";
import TypeQualifierAutocomplete from "../components/TypeQualifierAutocomplete";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "parentType", title: "Parent Type", inputType: TypeAutocomplete},
  {key: "requiredParentQualifier", title: "Required Parent Qualifier", inputType: TypeQualifierAutocomplete, inputProps: obj => {return {typeSymbol: obj.parentType}}},
  {key: "requiredParentObjects", title: "Required Parent Objects", inputType: TypeObjectsAutocomplete, inputProps: obj => {return {typeSymbol: obj.parentType}}},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "ownTermNames", title: "Qualifier term names (if not inheriting from parent)"},
  {key: "parentTerms", title: "Parent qualifier terms (if not inheriting)", inputType: InputWithShorthandReplacement},
  {key: "definingStatement", title: "Defining Statement", inputType: InputWithShorthandReplacement}
];

export default function PropertyDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveProperty = (typeToAdd) => {
    return context.updateChapter(path.join(context.url, "propertyDefinitions"), {method: "POST", body: typeToAdd});
  };
  return <ObjectInputWithSimpleForm description="Property Definition" values={values} save={saveProperty} onCancel={onCancel} />;
}
