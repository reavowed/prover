import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import TypeAutocomplete from "../components/TypeAutocomplete";
import TypeQualifierAutocomplete from "../components/TypeQualifierAutocomplete";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "parentType", title: "Parent Type", inputType: TypeAutocomplete},
  {key: "requiredParentQualifier", title: "Required Parent Qualifier", inputType: TypeQualifierAutocomplete, inputProps: obj => {return {typeSymbol: obj.parentType}}},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "mainVariableDefinition", title: "Main variable"},
  {key: "definingStatement", title: "Defining Statement", inputType: InputWithShorthandReplacement}
];

export default function RelatedObjectDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveDefinition = (typeToAdd) => {
    return context.updateChapter(path.join(context.url, "relatedObjectDefinitions"), {method: "POST", body: typeToAdd});
  };
  return <ObjectInputWithSimpleForm description="Related Object Definition" values={values} save={saveDefinition} onCancel={onCancel} />;
}
