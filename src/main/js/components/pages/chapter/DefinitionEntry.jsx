import React, {useContext, useState} from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {CopiableExpression} from "../../ExpressionComponent";
import ChapterContext from "./ChapterContext";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

function EditingShorthandButton({entry: {shorthand, url}}) {
  const [isEditing, setIsEditing] = useState(false);
  const [currentShorthand, setCurrentShorthand] = useState(shorthand);
  const context = useContext(ChapterContext);

  function saveShorthand() {
    context.updateChapter(url + "/shorthand", {
      method: "PUT",
      body: currentShorthand
    }).then(() => setIsEditing(false));
  }

  return <>
    <Button size="sm" variant="primary" className="ml-1 mb-n2" onClick={() => setIsEditing(true)}>Shorthand</Button>
    <Modal show={isEditing} onHide={() => setIsEditing(false)}>
      <Modal.Header closeButton><Modal.Title>Edit shorthand</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group>
            <Form.Label>Shorthand</Form.Label>
            <Form.Control type="text" value={currentShorthand} onChange={e => setCurrentShorthand(e.target.value)}/>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={() => setIsEditing(false)}>Close</Button>
        <Button variant="primary" onClick={saveShorthand}>Save Changes</Button>
      </Modal.Footer>
    </Modal>
  </>;
}

export default function DefinitionEntry({title, entry, children}) {
  const context = useContext(ChapterContext);
  return <>
    <ChapterEntryWrapper title={<>{title}: <CopiableExpression expression={entry.defaultValue} /></>}
                         url={entry.url}
                         buttons={context.editing && <EditingShorthandButton entry={entry} />}
    >
      {children}
    </ChapterEntryWrapper>
  </>;
}
