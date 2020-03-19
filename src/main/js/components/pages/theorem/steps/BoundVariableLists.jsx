import React, {useContext} from "react";

const Context = React.createContext();

Context.Add = function({variables, children}) {
  const existingLists = useContext(Context) || [];
  return <Context.Provider value={[...existingLists, variables]}>
    {children}
  </Context.Provider>
};
Context.AddParameters = function({variables, children}) {
  const existingLists = useContext(Context) || [];
  return <Context.Provider value={[variables, ...existingLists]}>
    {children}
  </Context.Provider>
};

export default Context;
