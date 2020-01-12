import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Proof from "./Proof";
import TheoremContext from "./TheoremContext";

export default function Proofs ({proofs}) {
  const context = useContext(TheoremContext);
  function addProof() {
    context.fetchJson("proofs", {method: "POST"})
      .then(context.updateTheorem);
  }
  return <>
    {_.map(proofs, (proof, index) => <Proof key={index}
                                            index={index}
                                            title={proofs.length > 1 ? `Proof ${index + 1}` : "Proof"}
                                            steps={proof}
                                            deleteable={proofs.length > 1}/>)}
    <Button size="sm" className="mt-3" onClick={addProof}>Add proof</Button>
  </>;
};

