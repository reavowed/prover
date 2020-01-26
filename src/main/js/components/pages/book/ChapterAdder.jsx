import path from "path";
import React, {useContext, useState} from "react";
import Button from "react-bootstrap/Button";
import ObjectInputWithSimpleForm from "../../helpers/ObjectInputWithSimpleForm";
import BookContext from "./BookContext";

const values = [
  {key: "title", title: "Title"},
  {key: "summary", title: "Summary", inputProps: {as: "textarea"}}
];

export function ChapterAdder() {
  const context = useContext(BookContext);
  const [adding, setAdding] = useState(false);
  const onCancel = () => setAdding(false);
  const saveChapter = (newChapter) => {
    return window.fetchJson(
      path.join(context.url, "chapters"),
      {method: "POST", body: newChapter}
    ).then(({chapters}) => context.updateChapters(chapters));
  };

  if (adding) {
    return <ObjectInputWithSimpleForm description="Chapter" values={values} save={saveChapter} onCancel={onCancel} />;
  } else {
    return <Button className="mr-2" onClick={() => setAdding(true)}>Add Chapter</Button>;
  }
}
