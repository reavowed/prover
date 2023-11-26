import React, {useContext} from "react";
import BoundVariableListContext from "./BoundVariableListContext";

export default function AddBoundVariableList({variables, children}: { variables: string[], children: React.ReactNode }) {
  const existingLists = useContext(BoundVariableListContext);
  return <BoundVariableListContext.Provider value={[...existingLists, variables]}>
    {children}
  </BoundVariableListContext.Provider>
}

