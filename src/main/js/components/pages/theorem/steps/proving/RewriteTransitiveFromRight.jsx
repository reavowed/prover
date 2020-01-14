import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";
import Rewriter from "./components/Rewriter";

export default function RewriteTransitiveFromRight({step, path}) {
  const context = useContext(ProofContext);
  const submit = (rewrites) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "rewriteRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }).then(() => this.setProvingType(null));
  };
  return <Rewriter
    title="Rewriting Right"
    expression={step.statement.components[1]}
    path={path}
    onSave={submit}/>
}
