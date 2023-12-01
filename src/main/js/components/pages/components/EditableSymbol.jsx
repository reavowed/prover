import * as path from "path";
import React from "react";
import {fetchJson} from "../../../utils";
import EditableProperty from "./EditableProperty";

export default function EditableSymbol({symbol, url}) {
  const saveSymbol = (newSymbol) => {
    return fetchJson(path.join(url, "symbol"), {method: "PUT", body: newSymbol})
      .then(url => window.location.pathname = url);
  };
  return <EditableProperty label="Symbol" initialValue={symbol} onSave={saveSymbol} />
}
