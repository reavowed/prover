import path from "path";
import React, {useContext} from "react";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "name", title: "Name"},
  {key: "premises", title: "Premises", inputType: InputWithShorthandReplacement, inputProps: {as: "textarea"}},
  {key: "conclusion", title: "Conclusion", inputType: InputWithShorthandReplacement},
];

export default function TheoremInput({onCancel}) {
  const context = useContext(ChapterContext);
  function saveTheorem(theoremToAdd) {
    theoremToAdd.premises = _.filter(theoremToAdd.premises.split(/\r?\n/), s => s.length);
    return context.updateChapter(path.join(context.url, "theorems"), {method: "POST", body: theoremToAdd});
  }
  return <ObjectInputWithSimpleForm description="Theorem" values={values} save={saveTheorem} onCancel={onCancel} />;
}
