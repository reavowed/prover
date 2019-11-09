import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {Parser} from "../Parser";

export class BoundVariableModal extends React.Component {
  constructor(...args) {
    super(...args);
    this.inputRef = React.createRef();
  }
  render() {
    const {show, onHide, title, label, value, onChange, onSave} = this.props;
    const onInputKeyUp = (event) => {
      if (event.keyCode === 13) {
        onSave();
      }
      event.preventDefault();
      event.stopPropagation();
    };
    return <Modal show={show} onHide={onHide} onEntered={() => {
      this.inputRef.current.focus();
      this.inputRef.current.select();
    }}>
      <Modal.Header closeButton><Modal.Title>{title}</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form.Group>
          <Form.Label>{label}</Form.Label>
          <Form.Control type="text"
                        value={value}
                        onChange={onChange}
                        onKeyUp={onInputKeyUp}
                        ref={this.inputRef}/>
        </Form.Group>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>Close</Button>
        <Button variant="primary" onClick={onSave}>Save Changes</Button>
      </Modal.Footer>
    </Modal>
  }
}
