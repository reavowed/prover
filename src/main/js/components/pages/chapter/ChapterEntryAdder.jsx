import React, {useState} from "react";
import Button from "react-bootstrap/Button";
import CommentInput from "./CommentInput";
import PropertyDefinitionInput from "./PropertyDefinitionInput";
import StandalonePropertyDefinitionInput from "./StandalonePropertyDefinitionInput";
import StatementDefinitionInput from "./StatementDefinitionInput";
import TermDefinitionInput from "./TermDefinitionInput";
import TheoremInput from "./TheoremInput";
import TypeDefinitionInput from "./TypeDefinitionInput";

export default function ChapterEntryAdder() {
  const [entryBeingAdded, setEntryBeingAdded] = useState(null);
  const onCancel = () => setEntryBeingAdded(null);
  const inputs = [
    {name: "Theorem", element: <TheoremInput onCancel={onCancel}/>},
    {name: "Statement Definition", element: <StatementDefinitionInput onCancel={onCancel}/>},
    {name: "Term Definition", element: <TermDefinitionInput onCancel={onCancel}/>},
    {name: "Type Definition", element: <TypeDefinitionInput onCancel={onCancel}/>},
    {name: "Property Definition", element: <PropertyDefinitionInput onCancel={onCancel}/>},
    {name: "Standalone Property Definition", element: <StandalonePropertyDefinitionInput onCancel={onCancel}/>},
    {name: "Comment", element: <CommentInput onCancel={onCancel}/>}
  ];
  if (!entryBeingAdded) {
    return <>
      {inputs.map(i => <Button className="mr-2 mb-2" key={i.name} onClick={() => setEntryBeingAdded(i.name)}>Add {i.name}</Button>)}
    </>
  } else {
    return _.find(inputs, i => i.name === entryBeingAdded).element;
  }
}
