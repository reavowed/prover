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
  {key: "text", title: "Comment text", inputProps: {as: "textarea"}}
];

export default function CommentInput({onCancel}) {
  const context = useContext(ChapterContext);
  const saveComment = ({text}) => {
    return context.updateChapter(path.join(context.url, "comments"), {method: "POST", body: text});
  };
  return <ObjectInputWithSimpleForm description="Comment" values={values} save={saveComment} onCancel={onCancel} />;
}
