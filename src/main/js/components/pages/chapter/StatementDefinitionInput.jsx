import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "components", title: "Components"},
  {key: "name", title: "Name"},
  {key: "format", title: "Format (in parens)"},
  {key: "definition", title: "Definition", inputType: InputWithShorthandReplacement},
  {key: "shorthand", title: "Shorthand"},
  {key: "attributes", title: "Attributes"},
];

export default function StatementDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  function saveTerm(termToAdd) {
    return context.updateChapter(path.join(context.url, "statementDefinitions"), {method: "POST", body: termToAdd});
  }
  return <ObjectInputWithSimpleForm description="Statement Definition" values={values} save={saveTerm} onCancel={onCancel} />;
}
