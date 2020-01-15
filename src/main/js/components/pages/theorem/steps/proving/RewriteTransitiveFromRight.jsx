import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";
import Rewriter from "./components/Rewriter";

export default function RewriteTransitiveFromRight({step, path, onError}) {
  const context = useContext(ProofContext);
  const submit = (rewrites) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "rewriteRight", {method: "POST", body: rewrites})
      .catch(onError);
  };
  return <Rewriter
    title="Rewriting Right"
    expression={step.statement.components[1]}
    path={path}
    onSave={submit}/>
}
