import _ from "lodash";
import React from "react";
import {Button, InputGroup} from "react-bootstrap";
import SimpleTextControl from "../../helpers/SimpleTextControl";

export default class EditableProperty extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      currentValue: props.initialValue || "",
      saving: false
    }
  }

  render() {
    const {label, onSave, onError, inputType, inputProps} = this.props;
    const {currentValue, saving} = this.state;

    const save = () => {
      const valueBeingSaved = currentValue;
      return this.setStatePromise({saving: true})
        .then(() => onSave(valueBeingSaved))
        .then(() => this.setStatePromise({savedValue: valueBeingSaved}))
        .catch((error) => {
          if (onError) {
            if (_.isObject(error)) {
              if (error.message)
                error = error.message;
              else {
                error = "Unknown error"
              }
            }
            onError(error);
          }
        })
        .then(() => this.setStatePromise({saving: false}))
    };

    const input = React.createElement(
      inputType || SimpleTextControl, {
        value: currentValue,
        onChange: (newValue) => this.setState({currentValue: newValue}),
        readOnly: !onSave || saving,
        ...(inputProps || {})
      });

    return <InputGroup className="mb-2">
      <InputGroup.Prepend><InputGroup.Text>{label}</InputGroup.Text></InputGroup.Prepend>
      {input}
      {onSave && <InputGroup.Append><Button variant="success" disabled={saving} onClick={save}><span className={saving ? "fas fa-spin fa-spinner" : "fas fa-check"}/></Button></InputGroup.Append>}
    </InputGroup>
  }
}
