import * as path from "path";
import React from "react";
import {createEntryPropertyUpdater} from "../utils/entryFunctions";
import EditableProperty from "./EditableProperty";

export default function EditableExplicitName({name, url}) {
  const saveName = createEntryPropertyUpdater(url, "name")
  return <EditableProperty label="Explicit Name" initialValue={name} onSave={saveName} />
}
