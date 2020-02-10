import path from "path";
import React from "react";
import {PremiseReference} from "../../models/Step";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {HighlightableExpression} from "../ExpressionComponent";
import {Inference} from "./Inference";
import Proofs from "./theorem/Proofs";
import TheoremContext from "./theorem/TheoremContext";

function Premise({statement, index}) {
  return <HighlightableExpression expression={statement} references={[new PremiseReference(index)]}/>
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      theorem: this.getParser().parseTheorem(props.theorem, props.inferences),
      inferences: props.inferences,
      highlighting: {
        actionHighlights: [],
        staticHighlights: [],
        isActionInUse: false
      },
      disableChaining: false
    }
  }

  onKeyDown = (event) => {
    if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) {
      return;
    }
    if (event.key === "d") {
      this.setState({disableChaining: !this.state.disableChaining});
    }
  };

  componentDidMount() {
    document.body.addEventListener('keydown', this.onKeyDown);
  }
  componentWillUnmount() {
    document.body.removeEventListener('keydown', this.onKeyDown);
  }

  getParser = () => new Parser(this.props.definitions, this.props.typeDefinitions);

  render() {
    const self = this;
    const {url, definitions, displayShorthands, definitionShorthands, binaryRelations} = this.props;
    const {theorem, inferences, highlighting, disableChaining} = this.state;
    const parser = this.getParser();

    const entryContext = {parser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};
    const theoremContext = {
      entryContext,
      parser,
      disableChaining,
      fetchJson(subpath,  options) {
        return window.fetchJson(path.join(url, subpath), options);
      },
      updateTheorem(newTheoremJson) {
        return new Promise((resolve) => {
          const newInferences = {...inferences, ...newTheoremJson.newInferences};
          self.setState({
            theorem: parser.parseTheorem(newTheoremJson.theorem, newInferences),
            inferences: newInferences
          }, () => resolve());
        })
      },
      updateStep(proofIndex, {path: stepPath, step: stepJson, newInferences: newInferencesFromStep, proof: proofJson}) {
        return new Promise((resolve) => {
          const newInferences = {...inferences, ...newInferencesFromStep};
          const newTheorem = proofJson ?
            self.state.theorem.updateProof(proofIndex, parser.parseSteps(proofJson, newInferences)) :
            self.state.theorem.updateStep(proofIndex, stepPath, parser.parseStep(stepJson, newInferences));
          self.setState({
            theorem: newTheorem,
            inferences: newInferences
          }, () => resolve());
        })
      },
      setHighlighting(newHighlightedPremises, newHighlightedConclusion, proofIndex) {
        if (!highlighting.isActionInUse) {
          self.setState({highlighting: {
              ...highlighting,
              actionHighlights: _.map(newHighlightedPremises, reference => {return {reference}}),
              staticHighlights: newHighlightedConclusion ? [newHighlightedConclusion] : [],
              proofIndex: proofIndex
            }});
        }
      },
      getHighlighting(proofIndex) {
        if (_.isNumber(proofIndex) && highlighting.proofIndex === proofIndex) {
          return [highlighting.actionHighlights, highlighting.staticHighlights];
        } else {
          return [_.filter(highlighting.actionHighlights, h => (h.reference instanceof PremiseReference)), []];
        }
      },
      setHighlightingAction(actionHighlights, staticHighlights, proofIndex) {
        self.setState({
          highlighting:  {
            actionHighlights,
            staticHighlights: staticHighlights || [],
            isActionInUse: true,
            proofIndex
          }
        });
      },
      clearHighlightingAction() {
        self.setState({
          highlighting:  {
            actionHighlights: [],
            staticHighlights: [],
            isActionInUse: false,
            proofIndex: null
          }
        });
      }
    };

    const createPremiseElement = (premise, index) => {
      return <Premise statement={premise} index={index}/>
    };

    return <EntryContext.Provider value={entryContext}>
      <TheoremContext.Provider value={theoremContext}>
        <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
          <Proofs proofs={theorem.proofs} />
        </Inference>
      </TheoremContext.Provider>
    </EntryContext.Provider> ;
  }
}
