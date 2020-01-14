import _ from "lodash";
import React, {useContext, useState} from "react";
import {Col, Row} from "react-bootstrap";
import Alert from "react-bootstrap/Alert";
import Button from "react-bootstrap/Button";
import styled from "styled-components";
import {matchTemplate} from "../../../../../models/Expression";
import EntryContext from "../../../../EntryContext";
import AddTarget from "./AddTarget";
import AddTransitiveTarget from "./AddTransitiveTarget";
import ApplyTransitivePremiseFromLeft from "./ApplyTransitivePremiseFromLeft";
import ApplyTransitivePremiseFromRight from "./ApplyTransitivePremiseFromRight";
import ProveTransitiveFromLeft from "./ProveTransitiveFromLeft";
import ProveTransitiveFromRight from "./ProveTransitiveFromRight";
import RearrangeAutomatically from "./RearrangeAutomatically";
import RewriteAutomatically from "./RewriteAutomatically";
import ExtractAutomatically from "./ExtractAutomatically";
import ExtractCurrentTarget from "./ExtractCurrentTarget";
import ExtractWithPremise from "./ExtractWithPremise";
import IntroduceBoundVariable from "./IntroduceBoundVariable";
import IntroduceDeduction from "./IntroduceDeduction";
import IntroduceName from "./IntroduceName";
import ProveCurrentTarget from "./ProveCurrentTarget";
import ProveNewPremise from "./ProveNewPremise";
import RewriteCurrentTarget from "./RewriteCurrentTarget";
import RewritePremise from "./RewritePremise";
import RewriteTransitiveFromLeft from "./RewriteTransitiveFromLeft";
import RewriteTransitiveFromRight from "./RewriteTransitiveFromRight";

export default function ProvingCard({step, path, availablePremises, transitive}) {
  const entryContext = useContext(EntryContext);
  const [currentProverLabel, setCurrentProverLabel] = useState(null);
  const [errorMessage, setErrorMessage] = useState(null);

  const scopingStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "scoping"));
  const deductionStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "deduction"));
  const binaryRelation = _.find(_.reverse(entryContext.binaryRelations.slice()), x => matchTemplate(x.template, step.statement, [], []));

  const onCancel = () => {
    setCurrentProverLabel(null);
    setErrorMessage(null);
  };
  const onError = (message) => {
    setErrorMessage(message);
  };
  const onErrorCancel = (message) => {
    onError(message);
    setCurrentProverLabel(null);
  };

  const rows = [
    {
      label: "Introduce",
      provers: [
        scopingStatement && step.statement.definition === scopingStatement && {
          label: "Bound variable",
          element: IntroduceBoundVariable
        },
        deductionStatement && step.statement.definition === deductionStatement && {
          label: "Deduction",
          element: IntroduceDeduction
        },
        {
          label: "Name",
          element: IntroduceName
        }
      ]
    },
    {
      label: "Prove",
      provers: [
        {
          label: "By inference",
          element: ProveCurrentTarget
        },
        {
          label: "By rewriting",
          element: RewriteCurrentTarget
        },
        {
          label: "By extracting",
          element: ExtractCurrentTarget
        }
      ]
    },
    !transitive && {
      label: "Insert before",
      provers: [
        {
          label: "New target",
          element: AddTarget
        },
        {
          label: "By inference",
          element: ProveNewPremise
        },
        {
          label: "Rewrite premise",
          element: RewritePremise
        },
        {
          label: "Extract with premise",
          element: ExtractWithPremise
        },
      ]
    },
    {
      label: "Automatic",
      provers: [
        {
          label: "Extract",
          element: ExtractAutomatically
        },
        {
          label: "Rewrite",
          element: RewriteAutomatically
        },
        {
          label: "Rearrange",
          element: RearrangeAutomatically
        }
      ]
    },
    binaryRelation && {
      label: "Insert transitive",
      provers: [
        {
          label: "Prove from left",
          element: ProveTransitiveFromLeft
        },
        {
          label: "Prove from right",
          element: ProveTransitiveFromRight
        },
        {
          label: "Rewrite left",
          element: RewriteTransitiveFromLeft
        },
        {
          label: "Rewrite right",
          element: RewriteTransitiveFromRight
        },
        {
          label: "Add premise from left",
          element: ApplyTransitivePremiseFromLeft
        },
        {
          label: "Add premise from right",
          element: ApplyTransitivePremiseFromRight
        },
        {
          label: "Target",
          element: AddTransitiveTarget
        }
      ]
    }
  ];
  const currentProver = _.chain(rows).filter().flatMap(r => r.provers).find(p => p.label === currentProverLabel).value();

  const SmallButton = styled(Button)`padding: 0.1rem 0.25rem;`;

  return <EntryContext.Consumer>{entryContext => {
    const proverProps = {step, path, availablePremises, onError, onCancel, onErrorCancel, entryContext};
    return <div>
      {errorMessage && <Alert variant="danger" onClose={() => setErrorMessage(null)} dismissible>{errorMessage}</Alert>}
      {currentProver ?
        <>
          <SmallButton variant="primary" className="float-right" onClick={onCancel}><span
            className="fas fa-undo fa-xs"/></SmallButton>
          {React.createElement(currentProver.element, proverProps)}
        </> :
        _.map(_.filter(rows), row => {
          const provers = _.filter(row.provers);
          if (provers.length)
            return <Row key={row.label} className="mb-1">
              <Col xs={2} className="text-right">{row.label}</Col>
              <Col xs={10}>
                {_.map(provers, prover =>
                  <Button key={prover.label} size="sm" className="ml-1 mb-1"
                          onClick={() => setCurrentProverLabel(prover.label)}>{prover.label}</Button>
                )}
              </Col>
            </Row>;
        })
      }
    </div>
  }}</EntryContext.Consumer>;
}
