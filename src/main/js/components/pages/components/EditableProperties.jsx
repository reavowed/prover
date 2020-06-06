import _ from "lodash";
import React, {useState} from "react";
import Alert from "react-bootstrap/Alert";
import {createEntryPropertyUpdater} from "../utils/entryFunctions";
import EditableProperty from "./EditableProperty";

export default function EditableProperties({url, updateEntry, definitions}) {
  const [errorMessage, setErrorMessage] = useState(null);

  const onError = (message) => {
    console.log(message);
    if (_.isObject(message)) {
      if (message.message)
        message = message.message;
      else {
        message = "Unknown error"
      }
    }
    setErrorMessage(message);
  };
  return <>
    <hr/>
    {errorMessage && <Alert variant="danger" className="mt-2" onClose={() => setErrorMessage(null)} dismissible>{errorMessage}</Alert>}
    {_.chain(definitions)
      .filter()
      .map(({label, initialValue, endpointName, process, ...otherProps}) => <EditableProperty key={label} label={label} initialValue={initialValue} onSave={endpointName && createEntryPropertyUpdater(url, endpointName, updateEntry, process)} onError={onError} {...otherProps} />)
      .value()}
  </>;
}
