import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import SingleActionProver from "./components/SingleActionProver";

export default function IntroduceBoundVariable({path, onErrorCancel}) {
  const context = useContext(ProofContext);
  const prove = () => context.fetchJsonForStepAndReplaceWithWrapping(path, "introduceBoundVariable", {method: "POST"});
  return <SingleActionProver prove={prove} onError={onErrorCancel} loadingText="Introducing bound variable" />
}
