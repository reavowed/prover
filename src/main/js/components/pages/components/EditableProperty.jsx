import React, {useState} from "react";
import {Button, InputGroup} from "react-bootstrap";
import Form from "react-bootstrap/Form";

export default class EditableProperty extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      savedValue: props.initialValue || "",
      currentValue: props.initialValue || "",
      saving: false
    }
  }

  render() {
    const {label, onSave, as} = this.props;
    const {currentValue, saving, savedValue} = this.state;

    const save = () => {
      const valueBeingSaved = currentValue;
      return this.setStatePromise({saving: true})
        .then(() => onSave(valueBeingSaved))
        .then(() => this.setStatePromise({savedValue: valueBeingSaved}))
        .catch(() => this.setStatePromise({currentValue: savedValue}))
        .then(() => this.setStatePromise({saving: false}))
    };

    return <InputGroup className="mb-2">
      <InputGroup.Prepend><InputGroup.Text>{label}</InputGroup.Text></InputGroup.Prepend>
      <Form.Control type="text" as={as} readOnly={saving} value={currentValue} onChange={e => this.setState({currentValue: e.target.value})}/>
      <InputGroup.Append><Button variant="success" disabled={saving} onClick={save}><span className={saving ? "fas fa-spin fa-spinner" : "fas fa-check"}/></Button></InputGroup.Append>
    </InputGroup>
  }
}
