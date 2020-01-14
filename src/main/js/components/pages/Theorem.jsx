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
      theorem: this.getParser().parseTheorem(props.theorem),
      inferences: this.props.inferences,
      highlighting: {
        actionHighlights: [],
        staticHighlights: [],
        isActionInUse: false
      }
    }
  }

  getParser = () => new Parser(this.props.definitions, this.props.typeDefinitions);

  render() {
    const self = this;
    const {url, definitions, displayShorthands, definitionShorthands, binaryRelations} = this.props;
    const {theorem, inferences, highlighting} = this.state;
    const parser = this.getParser();

    const entryContext = {parser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};
    const theoremContext = {
      parser,
      fetchJson(subpath,  options) {
        return window.fetch(path.join(url, subpath), options)
          .then(response => {
            return new Promise(((resolve, reject) => {
              response.json().then(response.ok ? resolve : reject);
            }))
          })
      },
      updateTheorem(newTheoremJson) {
        return new Promise((resolve) => {
          self.setState({
            theorem: parser.parseTheorem(newTheoremJson.theorem),
            inferences: {...inferences, ...newTheoremJson.newInferences}
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
