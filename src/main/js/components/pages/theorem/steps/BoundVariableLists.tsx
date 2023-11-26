import React, {useContext} from "react";

type ContextType = React.Context<string[][]> & {
  Add: typeof Add
  AddMultiple: typeof AddMultiple
  AddParameters: typeof AddParameters
}

const Context: ContextType = function() {
  let context: any = React.createContext<string[][]>([]);
  context.Add = Add;
  context.AddMultiple = AddMultiple;
  context.AddParameters = AddParameters;
  return context as ContextType;
}();

function Add({variables, children}: { variables: string[], children: React.ReactNode }) {
  const existingLists = useContext(Context);
  return <Context.Provider value={[...existingLists, variables]}>
    {children}
  </Context.Provider>
}

function AddMultiple({variables, children}: { variables: string[][], children: React.ReactNode }) {
  const existingLists = useContext(Context);
  return <Context.Provider value={[...existingLists, ...variables]}>
    {children}
  </Context.Provider>
}

function AddParameters({variables, children}: { variables: string[], children: React.ReactNode }) {
  const existingLists = useContext(Context);
  return <Context.Provider value={[variables, ...existingLists]}>
    {children}
  </Context.Provider>
}

export default Context;
