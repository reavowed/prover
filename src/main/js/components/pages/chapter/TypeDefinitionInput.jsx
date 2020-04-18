import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "defaultTermName", title: "Default term name"},
  {key: "qualifierTermNames", title: "Qualifier term names"},
  {key: "qualifierFormat", title: "Qualifier format (in parens)"},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definition", title: "Definition", inputType: InputWithShorthandReplacement}
];

export default function TypeDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveType = (propertyToAdd) => {
    return context.updateChapter(path.join(context.url, "typeDefinitions"), {method: "POST", body: propertyToAdd});
  };
  return <ObjectInputWithSimpleForm description="Type Definition" values={values} save={saveType} onCancel={onCancel} />;
}
