import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import SingleActionProver from "./components/SingleActionProver";

export default function IntroduceAll({path, onErrorCancel}) {
  const context = useContext(ProofContext);
  const prove = () => context.fetchJsonForStepAndReplaceWithWrapping(path, "introduceAll", {method: "POST"});
  return <SingleActionProver prove={prove} onError={onErrorCancel} loadingText="Introducing all generalizations and deductions" />
}
