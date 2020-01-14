import {default as React, useContext} from "react";
import ProofContext from "../../ProofContext";
import SingleActionProver from "./components/SingleActionProver";

export default function RewriteAutomatically({path, onErrorCancel}) {
  const context = useContext(ProofContext);
  const prove = () => context.fetchJsonForStepAndUpdateTheorem(path, "rewriteAutomatically", {method: "POST"});
  return <SingleActionProver prove={prove} onError={onErrorCancel} loadingText="Rewriting" />
}
