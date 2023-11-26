import React, {useContext} from "react";
import BoundVariableListContext from "./BoundVariableListContext";

export default function AddParameterList({parameters, children}: { parameters: string[], children: React.ReactNode }) {
  const existingLists = useContext(BoundVariableListContext);
  return <BoundVariableListContext.Provider value={[parameters, ...existingLists]}>
    {children}
  </BoundVariableListContext.Provider>
}
