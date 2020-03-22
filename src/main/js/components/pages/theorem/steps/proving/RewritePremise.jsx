import React, {useContext, useState} from "react";
import ProofContext from "../../ProofContext";
import PremiseChooser from "./components/PremiseChooser";
import Rewriter from "./components/Rewriter";

export default function RewritePremise({path, availablePremises, entryContext, onCancel, onError}) {
  const context = useContext(ProofContext);
  const [premiseToRewrite, setPremiseToRewrite] = useState(null);

  const rewrite = (rewrites) => {
    return context.fetchJsonForStepAndInsert(path, "rewritePremise", {method: "POST", body: rewrites})
      .catch(onError);
  };
  return <>
    <PremiseChooser premise={premiseToRewrite} setPremise={setPremiseToRewrite} availablePremises={availablePremises} entryContext={entryContext}/>
    {premiseToRewrite && <Rewriter title="Rewriting Premise"
                                   expression={premiseToRewrite.statement}
                                   path={path}
                                   onSave={rewrites => rewrite({serializedPremise: premiseToRewrite.statement.serialize(), rewrites})}
    />}
  </>;
}
