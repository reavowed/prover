import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import Rewriter from "./components/Rewriter";

export default function RewriteCurrentTarget({step, path, onError}) {
  const context = useContext(ProofContext);
  const rewrite = (rewrites) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "rewrite", {method: "POST", body: rewrites})
      .catch(onError);
  };
  return <Rewriter
    title="Rewriting"
    expression={step.statement}
    path={path}
    onSave={rewrite}
  />
}