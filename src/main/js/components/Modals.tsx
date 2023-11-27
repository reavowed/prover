import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";

type SimpleModalProps = {
  show: boolean
  onHide: () => void
  title: string
  value: string
  onChange: (newValue: string) => void
  onSave: () => void
}
export function SimpleModal(props: SimpleModalProps) {
  const {show, onHide, title, value, onChange, onSave} = props;
  const ref = React.useRef<HTMLInputElement>(null)
  const onInputKeyUp = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      onSave();
    }
    event.preventDefault();
    event.stopPropagation();
  };
  return <Modal show={show} onHide={onHide} onEntered={() => {
    ref.current!.focus();
    ref.current!.select();
  }}>
    <Modal.Header closeButton><Modal.Title>{title}</Modal.Title></Modal.Header>
    <Modal.Body>
      <Form.Group>
        <Form.Control type="text"
                      value={value}
                      onChange={e => onChange(e.target.value)}
                      onKeyUp={onInputKeyUp}
                      ref={ref}/>
      </Form.Group>
    </Modal.Body>
    <Modal.Footer>
      <Button variant="secondary" onClick={onHide}>Close</Button>
      <Button variant="primary" onClick={onSave}>Save Changes</Button>
    </Modal.Footer>
  </Modal>
}
