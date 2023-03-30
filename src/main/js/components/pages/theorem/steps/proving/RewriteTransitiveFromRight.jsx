import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";
import Rewriter from "./components/Rewriter";

export default function RewriteTransitiveFromRight({path, rightComponent, onError, availableEntries}) {
  const context = useContext(ProofContext);
  const submit = (rewrites) => {
    return context.fetchJsonForStepAndInsertAndReplaceMultiple(path, "rewriteRight", {method: "POST", body: rewrites})
      .catch(onError);
  };
  return <Rewriter
    title="Rewriting Right"
    expression={rightComponent}
    path={path}
    onSave={submit}
    availableEntries={availableEntries}/>
}
