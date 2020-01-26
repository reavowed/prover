import React, {useState} from "react";
import Button from "react-bootstrap/Button";
import CommentInput from "./CommentInput";
import PropertyInput from "./PropertyInput";
import TermInput from "./TermInput";
import TheoremInput from "./TheoremInput";
import TypeInput from "./TypeInput";

export default function ChapterEntryAdder() {
  const [entryBeingAdded, setEntryBeingAdded] = useState(null);
  const onCancel = () => setEntryBeingAdded(null);
  const inputs = [
    {name: "Theorem", element: <TheoremInput onCancel={onCancel}/>},
    {name: "Term", element: <TermInput onCancel={onCancel}/>},
    {name: "Type", element: <TypeInput onCancel={onCancel}/>},
    {name: "Property", element: <PropertyInput onCancel={onCancel}/>},
    {name: "Comment", element: <CommentInput onCancel={onCancel}/>}
  ];
  if (!entryBeingAdded) {
    return <>
      {inputs.map(i => <Button className="mr-2" key={i.name} onClick={() => setEntryBeingAdded(i.name)}>Add {i.name}</Button>)}
    </>
  } else {
    return _.find(inputs, i => i.name === entryBeingAdded).element;
  }
}
