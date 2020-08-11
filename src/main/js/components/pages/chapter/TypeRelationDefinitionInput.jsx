import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import TypeAutocomplete from "../components/TypeAutocomplete";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "firstType", title: "First Type", inputType: TypeAutocomplete},
  {key: "firstTypeVariable", title: "First type variable"},
  {key: "firstTypeQualifierTerms", title: "First type qualifier terms", inputType: InputWithShorthandReplacement},
  {key: "secondType", title: "Second Type", inputType: TypeAutocomplete},
  {key: "secondTypeVariable", title: "Second variable"},
  {key: "secondTypeQualifierTerms", title: "Second type qualifier terms", inputType: InputWithShorthandReplacement},
  {key: "linkingPhrase", title: "Linking phrase"},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definingStatement", title: "Definition", inputType: InputWithShorthandReplacement}
];

export default function TypeRelationDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveType = (propertyToAdd) => {
    return context.updateChapter(path.join(context.url, "typeRelationDefinitions"), {method: "POST", body: propertyToAdd});
  };
  return <ObjectInputWithSimpleForm description="Type Relation Definition" values={values} save={saveType} onCancel={onCancel} />;
}
