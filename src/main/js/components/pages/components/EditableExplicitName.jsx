import * as path from "path";
import React from "react";
import EditableProperty from "./EditableProperty";

export default function EditableExplicitName({name, url}) {
  const saveName = (newName) => {
    return window.fetchJson(path.join(url, "name"), {method: "PUT", body: newName})
      .then(url => window.location.pathname = url);
  };
  return <EditableProperty label="Explicit Name" initialValue={name} onSave={saveName} />
}
