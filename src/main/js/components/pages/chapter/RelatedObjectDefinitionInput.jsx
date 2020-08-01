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
  {key: "mainVariableDefinition", title: "Main variable"},
  {key: "parentType", title: "Parent Type", inputType: TypeAutocomplete},
  {key: "requiredParentQualifier", title: "Required Parent Qualifier", inputType: TypeQualifierAutocomplete, inputProps: obj => {return {typeSymbol: obj.parentType}}},
  {key: "requiredParentObjects", title: "Required Parent Objects", inputType: TypeObjectsAutocomplete, inputProps: obj => {return {typeSymbol: obj.parentType}}},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definingStatement", title: "Defining Statement", inputType: InputWithShorthandReplacement}
];

export default function RelatedObjectDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveDefinition = (typeToAdd) => {
    return context.updateChapter(path.join(context.url, "relatedObjectDefinitions"), {method: "POST", body: typeToAdd});
  };
  return <ObjectInputWithSimpleForm description="Related Object Definition" values={values} save={saveDefinition} onCancel={onCancel} />;
}
