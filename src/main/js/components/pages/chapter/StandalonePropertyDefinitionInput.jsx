import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "defaultTermName", title: "Default term name"},
  {key: "otherTermNames", title: "Other term names"},
  {key: "format", title: "Format (in parens)"},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definingStatement", title: "Defining Statement", inputType: InputWithShorthandReplacement}
];

export default function StandalonePropertyDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveProperty = (typeToAdd) => {
    return context.updateChapter(path.join(context.url, "standalonePropertyDefinitions"), {method: "POST", body: typeToAdd});
  };
  return <ObjectInputWithSimpleForm description="Standalone Property Definition" values={values} save={saveProperty} onCancel={onCancel} />;
}
