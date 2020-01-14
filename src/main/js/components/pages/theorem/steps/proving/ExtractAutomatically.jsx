import {default as React, useContext} from "react";
import ProofContext from "../../ProofContext";
import SingleActionProver from "./components/SingleActionProver";

export default function ExtractAutomatically({path, onErrorCancel}) {
  const context = useContext(ProofContext);
  const prove = () => context.fetchJsonForStepAndUpdateTheorem(path, "extractAutomatically", {method: "POST"});
  return <SingleActionProver prove={prove} onError={onErrorCancel} loadingText="Extracting" />
}
