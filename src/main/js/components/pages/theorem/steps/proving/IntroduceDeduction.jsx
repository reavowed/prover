import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import SingleActionProver from "./components/SingleActionProver";

export default function IntroduceDeduction({path, onErrorCancel}) {
  const context = useContext(ProofContext);
  const prove = () => context.fetchJsonForStepAndReplaceWithWrapping(path, "introduceDeduction", {method: "POST"})
    .then(() => context.callOnStep([...path, 0], "startProving"));
  return <SingleActionProver prove={prove} onError={onErrorCancel} loadingText="Introducing deduction" />
}
