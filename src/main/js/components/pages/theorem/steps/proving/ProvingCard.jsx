import _ from "lodash";
import React, {createRef} from "react";
import {ButtonToolbar, Col, Row} from "react-bootstrap";
import Alert from "react-bootstrap/Alert";
import Button from "react-bootstrap/Button";
import styled from "styled-components";
import {matchTemplate} from "../../../../../models/Expression";
import EntryContext from "../../../../EntryContext";
import {CopiableExpression} from "../../../../ExpressionComponent";
import AddTargetByInference from "./AddTargetByInference";
import AddTargetByPremise from "./AddTargetByPremise";
import AddTargetManually from "./AddTargetManually";
import AddChainedTarget from "./AddChainedTarget";
import ApplyChainingPremiseFromLeft from "./ApplyChainingPremiseFromLeft";
import ApplyChainingPremiseFromRight from "./ApplyChainingPremiseFromRight";
import IntroduceAll from "./IntroduceAll";
import IntroduceBoundVariable from "./IntroduceBoundVariable";
import IntroduceDeduction from "./IntroduceDeduction";
import IntroduceName from "./IntroduceName";
import IntroduceNameByInference from "./IntroduceNameByInference";
import ProveChainingFromLeft from "./ProveChainingFromLeft";
import ProveChainingFromRight from "./ProveChainingFromRight";
import ProveCurrentTargetByInference from "./ProveCurrentTargetByInference";
import ProveCurrentTargetByPremise from "./ProveCurrentTargetByPremise";
import RearrangeAutomatically from "./RearrangeAutomatically";
import RewriteAutomatically from "./RewriteAutomatically";
import RewriteCurrentTarget from "./RewriteCurrentTarget";
import RewriteCurrentTargetByDefinition from "./RewriteCurrentTargetByDefinition";
import RewritePremise from "./RewritePremise";
import RewriteTransitiveFromLeft from "./RewriteTransitiveFromLeft";
import RewriteTransitiveFromRight from "./RewriteTransitiveFromRight";

export default class ProvingCard extends React.Component {
  static contextType = EntryContext;
  constructor(props) {
    super(props);
    this.ref = createRef();
    this.state = {
      currentRowLabel: null,
      currentProverLabel: null,
      errorMessage: null
    }
  }

  componentDidMount() {
    this.ref.current.focus();
  }

  onCancel = () => {
    this.setState({errorMessage: null, currentProverLabel: null});
  };
  onError = (message) => {
    console.log(message);
    if (_.isObject(message)) {
      if (message.message)
        message = message.message;
      else {
        message = "Unknown error"
      }
    }
    this.setState({errorMessage: message});
  };
  onErrorCancel = (message) => {
    this.onError(message);
    this.setState({currentProverLabel: null});
  };

  render() {
    const {step, path, availablePremises, chained} = this.props;
    const {currentRowLabel, currentProverLabel, errorMessage} = this.state;

    const generalizationStatement = _.find(this.context.definitions, d => _.includes(d.attributes, "generalization"));
    const deductionStatement = _.find(this.context.definitions, d => _.includes(d.attributes, "deduction"));
    const [binaryRelation, leftComponent, rightComponent] = _.chain(this.context.binaryRelations)
      .map(x => {
        const matchResult = matchTemplate(x.template, step.statement, [], []);
        return matchResult ? [x, matchResult[0].expression, matchResult[1].expression] : null;
      })
      .find()
      .value() || [null, null, null];

    const isGeneralization = generalizationStatement && step.statement.definition === generalizationStatement;
    const isDeduction = deductionStatement && step.statement.definition === deductionStatement;

    const rows = [
      {
        label: "Introduce",
        provers: [
          isGeneralization && {
            label: "Bound variable",
            element: IntroduceBoundVariable
          },
          isDeduction && {
            label: "Deduction",
            element: IntroduceDeduction
          },
          (isGeneralization || isDeduction) && {
            label: "All",
            element: IntroduceAll
          },
          {
            label: "Name",
            element: IntroduceName
          },
          {
            label: "Name by inference",
            element: IntroduceNameByInference
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
            element: RewriteCurrentTarget
          },
          {
            label: "By rewriting definition",
            element: RewriteCurrentTargetByDefinition
          }
        ]
      },
      {
        label: "Insert target",
        provers: [
          {
            label: "By inference",
            element: AddTargetByInference
          },
          {
            label: "By premise",
            element: AddTargetByPremise
          },
          {
            label: "By rewriting equality",
            element: RewritePremise
          },
          {
            label: "Manually",
            element: AddTargetManually
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
            element: ProveChainingFromLeft
          },
          {
            label: "Prove from right",
            element: ProveChainingFromRight
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
            element: ApplyChainingPremiseFromLeft
          },
          {
            label: "Add premise from right",
            element: ApplyChainingPremiseFromRight
          },
          {
            label: "Target",
            element: AddChainedTarget
          }
        ]
      }
    ];
    const currentRow = _.find(rows, r => r && r.label === currentRowLabel);
    const currentProver = currentRow && _.find(currentRow.provers, p => p && p.label === currentProverLabel);

    return <EntryContext.Consumer>{entryContext => {
      const proverProps = {
        step,
        path,
        availablePremises,
        leftComponent,
        rightComponent,
        onError: this.onError,
        onCancel: this.onCancel,
        onErrorCancel: this.onErrorCancel,
        entryContext
      };
      return <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}} ref={this.ref} tabIndex={0}>
        <ButtonToolbar style={{position:"absolute"}}>
          <Button size="sm" variant="danger" onClick={this.props.stopProving}><i className="fas fa-times"/></Button>
          {currentProver && <Button size="sm" variant="primary" onClick={this.onCancel} className="ml-1"><i className="fas fa-undo"/></Button>}
        </ButtonToolbar>
        <h5 className="text-center">
          <CopiableExpression expression={step.statement} />
        </h5>
        {errorMessage &&
        <Alert variant="danger" onClose={() => this.setState({errorMessage: null})} dismissible>{errorMessage}</Alert>}
        {currentProver ?
          React.createElement(currentProver.element, proverProps) :
          <div className="container">
            {_.map(_.filter(rows), row => {
              const provers = _.filter(row.provers);
              if (provers.length)
                return <Row key={row.label} className="mb-1">
                  <Col xs={2} className="text-right">{row.label}</Col>
                  <Col xs={10}>
                    {_.map(provers, prover =>
                      <Button key={prover.label}
                              size="sm"
                              className="ml-1 mb-1"
                              onClick={() => {
                                this.setState({currentRowLabel: row.label, currentProverLabel: prover.label});
                              }}
                      >
                        {prover.label}
                      </Button>
                    )}
                  </Col>
                </Row>;
            })}
          </div>
        }
      </div>;
    }}</EntryContext.Consumer>;
  }
}
