import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "parentType", title: "Parent Type"},
  {key: "defaultTermName", title: "Default term name"},
  {key: "parentComponents", title: "Parent components"},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definingStatement", title: "Defining Statement", inputType: InputWithShorthandReplacement}
];

export default function PropertyDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveProperty = (typeToAdd) => {
    return context.updateChapter(path.join(context.url, "propertyDefinitions"), {method: "POST", body: typeToAdd});
  };
  return <ObjectInputWithSimpleForm description="Property Definition" values={values} save={saveProperty} onCancel={onCancel} />;
}
