import React, {useContext} from "react";
import BoundVariableListContext from "./BoundVariableListContext";

export default function AddBoundVariableLists({variableLists, children}: { variableLists: string[][], children: React.ReactNode }) {
  const existingLists = useContext(BoundVariableListContext);
  return <BoundVariableListContext.Provider value={[...existingLists, ...variableLists]}>
    {children}
  </BoundVariableListContext.Provider>
}
