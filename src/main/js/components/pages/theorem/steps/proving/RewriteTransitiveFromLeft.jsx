import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";
import Rewriter from "./components/Rewriter";

export default function RewriteTransitiveFromLeft({path, leftComponent, onError, availableEntries}) {
  const context = useContext(ProofContext);
  const submit = (rewrites) => {
    return context.fetchJsonForStepAndInsertAndReplaceMultiple(path, "rewriteLeft", {method: "POST", body: rewrites})
      .catch(onError);
  };
  return <Rewriter
    title="Rewriting Left"
    expression={leftComponent}
    path={path}
    onSave={submit}
    availableEntries={availableEntries}/>
}
