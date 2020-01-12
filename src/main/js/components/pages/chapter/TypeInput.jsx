import path from "path";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {Parser} from "../../../Parser";
import InputWithShorthandReplacement from "../../helpers/InputWithShorthandReplacement";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import SimpleControlGroup from "../../helpers/SimpleControlGroup";
import ChapterContext from "./ChapterContext";

const values = [
  {key: "symbol", title: "Symbol"},
  {key: "defaultTermName", title: "Default term name"},
  {key: "otherComponents", title: "Other components"},
  {key: "format", title: "Format (in parens)"},
  {key: "name", title: "Explicit name (if different from symbol)"},
  {key: "definition", title: "Definition", inputType: InputWithShorthandReplacement}
];

export default function TypeInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveType = (propertyToAdd) => {
    return context.updateChapter(path.join(context.url, "typeDefinitions"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(propertyToAdd)
    });
  };
  return <ObjectInputWithSimpleForm description="Type" values={values} save={saveType} onCancel={onCancel} />;
}
