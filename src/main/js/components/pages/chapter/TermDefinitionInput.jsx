import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "disambiguator", title: "Disambiguator"},
  {key: "components", title: "Components"},
  {key: "name", title: "Name"},
  {key: "format", title: "Format (in parens)"},
  {key: "premises", title: "Premises", inputType: InputWithShorthandReplacement, inputProps: {as: "textarea"}},
  {key: "definition", title: "Definition ('_' as placeholder)", inputType: InputWithShorthandReplacement},
  {key: "shorthand", title: "Shorthand"},
  {key: "attributes", title: "Attributes"},
];

export default function TermDefinitionInput({onCancel}) {
  const context = useContext(ChapterContext);
  function saveTerm(termToAdd) {
    termToAdd.premises = _.filter(termToAdd.premises.split(/\r?\n/), s => s.length);
    return context.updateChapter(path.join(context.url, "termDefinitions"), {method: "POST", body: termToAdd});
  }
  return <ObjectInputWithSimpleForm description="Term Definition" values={values} save={saveTerm} onCancel={onCancel} />;
}
