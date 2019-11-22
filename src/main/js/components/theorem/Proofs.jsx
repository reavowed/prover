import React from "react";
import Button from "react-bootstrap/Button";
import {connect} from "react-redux";
import Proof from "./Proof";
import {FetchJsonAndUpdate} from "./TheoremStore";

const ProofFromIndex = connect(
  (state, {index}) => {
    return {
      title: state.theorem.proofs.length > 1 ? `Proof ${index + 1}` : "Proof",
      steps: state.theorem.proofs[index],
      index: index,
      theoremUrl: state.url,
      deleteable: state.theorem.proofs.length > 1
    }
  }
)(Proof);

const Proofs = ({numberOfProofs, dispatch}) => {
  function addProof() {
    dispatch(FetchJsonAndUpdate("proofs", {method: "POST"}));
  }
  return <>
    {_.map(_.range(numberOfProofs), index => <ProofFromIndex key={index} index={index}/>)}
    <Button size="sm" className="mt-3" onClick={addProof}>Add proof</Button>
  </>;
};

export default connect(
  state => {
    return {
      numberOfProofs: state.theorem.proofs.length
    }
  }
)(Proofs);
