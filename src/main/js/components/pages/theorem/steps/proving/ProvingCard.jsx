import _ from "lodash";
import React, {useContext, useState} from "react";
import {Col, Row} from "react-bootstrap";
import Alert from "react-bootstrap/Alert";
import Button from "react-bootstrap/Button";
import styled from "styled-components";
import {matchTemplate} from "../../../../../models/Expression";
import EntryContext from "../../../../EntryContext";
import AddTargetManually from "./AddTargetManually";
import AddTransitiveTarget from "./AddTransitiveTarget";
import ApplyTransitivePremiseFromLeft from "./ApplyTransitivePremiseFromLeft";
import ApplyTransitivePremiseFromRight from "./ApplyTransitivePremiseFromRight";
import ProveCurrentTargetByPremise from "./ProveCurrentTargetByPremise";
import ProveTransitiveFromLeft from "./ProveTransitiveFromLeft";
import ProveTransitiveFromRight from "./ProveTransitiveFromRight";
import RearrangeAutomatically from "./RearrangeAutomatically";
import RewriteAutomatically from "./RewriteAutomatically";
import IntroduceBoundVariable from "./IntroduceBoundVariable";
import IntroduceDeduction from "./IntroduceDeduction";
import IntroduceName from "./IntroduceName";
import ProveCurrentTargetByInference from "./ProveCurrentTargetByInference";
import AddTargetByInference from "./AddTargetByInference";
import ProveCurrentTargetByRewritingEquality from "./RewriteCurrentTarget";
import ProveCurrentTargetByRewritingDefinition from "./RewriteCurrentTargetByDefinition";
import AddTargetByRewritingEquality from "./RewriteEqualityFromPremise";
import RewriteTransitiveFromLeft from "./RewriteTransitiveFromLeft";
import RewriteTransitiveFromRight from "./RewriteTransitiveFromRight";

export default function ProvingCard({step, path, availablePremises, chained}) {
  const entryContext = useContext(EntryContext);
  const [currentRowLabel, setCurrentRowLabel] = useState(null);
  const [currentProverLabel, setCurrentProverLabel] = useState(null);
  const [errorMessage, setErrorMessage] = useState(null);

  const scopingStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "scoping"));
  const deductionStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "deduction"));
  const [binaryRelation, leftComponent, rightComponent] = _.chain(entryContext.binaryRelations)
    .map(x => {
      const matchResult = matchTemplate(x.template, step.statement, [], []);
      return matchResult ? [x, matchResult[0].expression, matchResult[1].expression] : null;
    })
    .find()
    .value() || [null, null, null];


  const onCancel = () => {
    setCurrentProverLabel(null);
    setErrorMessage(null);
  };
  const onError = (message) => {
    console.log(message);
    if (_.isObject(message)) {
      if (message.message)
        message = message.message;
      else {
        message = "Unknown error"
      }
    }
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
      label: "Prove this",
      provers: [
        {
          label: "By inference",
          element: ProveCurrentTargetByInference
        },
        {
          label: "By premise",
          element: ProveCurrentTargetByPremise
        },
        {
          label: "By rewriting equality",
          element: ProveCurrentTargetByRewritingEquality
        },
        {
          label: "By rewriting definition",
          element: ProveCurrentTargetByRewritingDefinition
        }
      ]
    },
    !chained && {
      label: "Insert target",
      provers: [
        {
          label: "By inference",
          element: AddTargetByInference
        },
        {
          label: "Manually",
          element: AddTargetManually
        },
        {
          label: "By rewriting equality",
          element: AddTargetByRewritingEquality
        }
      ]
    },
    {
      label: "Automatic",
      provers: [
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
  const currentRow = _.find(rows, r => r && r.label === currentRowLabel);
  const currentProver = currentRow && _.find(currentRow.provers, p => p && p.label === currentProverLabel);

  const SmallButton = styled(Button)`padding: 0.1rem 0.25rem;`;

  return <EntryContext.Consumer>{entryContext => {
    const proverProps = {step, path, availablePremises, leftComponent, rightComponent, onError, onCancel, onErrorCancel, entryContext};
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
                  <Button key={prover.label}
                          size="sm"
                          className="ml-1 mb-1"
                          onClick={() => {setCurrentRowLabel(row.label); setCurrentProverLabel(prover.label);}}
                  >
                    {prover.label}
                  </Button>
                )}
              </Col>
            </Row>;
        })
      }
    </div>
  }}</EntryContext.Consumer>;
}
