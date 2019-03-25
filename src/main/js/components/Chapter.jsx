import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import styled from "styled-components";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {Expression} from "./Expression";
import {FlexRow} from "./FlexRow";
import {InferenceSummary} from "./InferenceSummary";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {ResultWithPremises} from "./ResultWithPremises";

const ResultWrapper = styled.div`
  margin: 10px 0;
  padding: 5px 10px;
  border: 1px solid rgba(0,0,0,0.5);
  border-radius: 5px;
`;

const ResultTitle = styled.a`
  color: black;
  font-weight: bold;
  
  &:hover {
    color: black;
  }
`;

const Result = ({title, href, buttons, children}) => <ResultWrapper>
  <FlexRow>
    <FlexRow.Grow><ResultTitle href={href}>{title}</ResultTitle></FlexRow.Grow>
    {buttons}
  </FlexRow>
  {children}
</ResultWrapper>;

const InferenceResult = ({title, entry, updateChapter}) => {
  const deleteInference = () => {
    updateChapter(entry.key.value, {method: "DELETE"})
  };
  return <Result title={<>{title}: {entry.name}</>}
                 href={entry.key.url}
                 buttons={<Button size="sm" variant="danger" className="ml-1" onClick={deleteInference}><span className="fas fa-ban"/></Button>}>
    <InferenceSummary inference={entry}/>
  </Result>;
};

class DefinitionResult extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      editingShorthand: false,
      shorthand: props.entry.shorthand
    }
  }
  startEditingShorthand = () => {
    this.setState({editingShorthand: true})
  };
  stopEditingShorthand = () => {
    this.setState({editingShorthand: false})
  };
  updateShorthandState = (newShorthand) => {
    this.setState({shorthand: newShorthand})
  };
  saveShorthand = () => {
    this.props
      .updateChapter(this.props.entry.key.value + "/shorthand", {
        method: "PUT",
        body: this.state.shorthand
      })
      .then(() => this.stopEditingShorthand());
  };
  render() {
    const {title, entry, children} = this.props;

    return <>
      <Result title={<>{title}: <Expression expression={entry.defaultValue} boundVariableLists={[]}/></>}
              buttons={<Button size="sm" variant="primary" className="ml-1" onClick={this.startEditingShorthand}>Shorthand</Button>}
      >
        {children && <div className="mt-n2">
          {children}
        </div>}
      </Result>
      <Modal show={this.state.editingShorthand} onHide={this.stopEditingShorthand}>
        <Modal.Header closeButton><Modal.Title>Edit shorthand</Modal.Title></Modal.Header>
        <Modal.Body>
          <Form>
            <Form.Group>
              <Form.Label>Shorthand</Form.Label>
              <Form.Control type="text" value={this.state.shorthand} onChange={e => this.updateShorthandState(e.target.value)}/>
            </Form.Group>
          </Form>
        </Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={this.stopEditingShorthand}>Close</Button>
          <Button variant="primary" onClick={this.saveShorthand}>Save Changes</Button>
        </Modal.Footer>
      </Modal>
    </>;
  }
};

export class Chapter extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      entries: Parser.parseEntries(props.entries),
      addingTheorem: false,
      theoremBeingAdded: {
        name: "",
        premises: "",
        conclusion: ""
      }
    }
  }

  updateChapter = (fetchUrl, fetchData) => {
    return window.fetch(path.join(this.props.chapterKey.url, fetchUrl), fetchData)
      .then(response => {
        if (response.ok) {
          return response.json()
        } else {
          throw response.statusText;
        }
      })
      .then(newProps => {
        this.setState({entries: Parser.parseEntries(newProps.entries)});
      });
  };
  renderEntry = (entry) => {
    switch (entry.type) {
      case "axiom":
        return <InferenceResult key={entry.key.value} title="Axiom" entry={entry} updateChapter={this.updateChapter}/>;
      case "theorem":
        return <InferenceResult key={entry.key.value} title="Theorem" entry={entry} updateChapter={this.updateChapter}/>;
      case "statementDefinition":
        return <DefinitionResult key={entry.key.value} title="Statement Definition" entry={entry} updateChapter={this.updateChapter}>
          {entry.definingStatement && <><Expression expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <Expression expression={entry.definingStatement} boundVariableLists={[]}/>.</>}
        </DefinitionResult>;
      case "termDefinition":
        return <DefinitionResult key={entry.key.value} title="Term Definition" entry={entry} updateChapter={this.updateChapter}>
          <ResultWithPremises premises={entry.premises}
                              result={<><Expression expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <Expression expression={entry.definingStatement} boundVariableLists={[]}/></>}/>
        </DefinitionResult>;
      case "comment":
        return <p key={entry.key.value}>{entry.text}</p>
      default:
        return <React.Fragment key={entry.key.value}/>
    }
  };
  startAddingTheorem = () => {
    this.setState({
      addingTheorem: true,
      theoremBeingAdded: {
        name: "",
        premises: "",
        conclusion: ""
      }
    })
  };
  stopAddingTheorem = () => {
    this.setState({addingTheorem: false})
  };
  updateTheoremName = (newName) => {
    const {theoremBeingAdded} = this.state;
    theoremBeingAdded.name = newName;
    this.setState({theoremBeingAdded});
  };
  updateTheoremPremises = (newPremises) => {
    const {theoremBeingAdded} = this.state;
    theoremBeingAdded.premises = newPremises;
    this.setState({theoremBeingAdded});
  };
  updateTheoremConclusion = (newConclusion) => {
    const {theoremBeingAdded} = this.state;
    theoremBeingAdded.conclusion = newConclusion;
    this.setState({theoremBeingAdded});
  };
  saveTheorem = () => {
    const {theoremBeingAdded} = this.state;
    const theoremToAdd = _.cloneDeep(theoremBeingAdded);
    theoremToAdd.premises = _.filter(theoremToAdd.premises.split(/\r?\n/), s => s.length);
    this.updateChapter("theorems", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(theoremToAdd)
    }).then(this.stopAddingTheorem);
  };

  render() {
    const {title, chapterKey, summary, previous, next} = this.props;
    const {entries} = this.state;
    return <Page breadcrumbs={<Breadcrumbs.Chapter chapterKey={chapterKey}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}</h3>
      <p>{summary}</p>
      {entries.map(this.renderEntry)}
      <Button onClick={this.startAddingTheorem}>Add theorem</Button>
      <Modal show={this.state.addingTheorem} onHide={this.stopAddingTheorem}>
        <Modal.Header closeButton><Modal.Title>Add theorem</Modal.Title></Modal.Header>
        <Modal.Body>
          <Form>
            <Form.Group>
              <Form.Label>Name</Form.Label>
              <Form.Control type="text" value={this.state.theoremBeingAdded.name} onChange={e => this.updateTheoremName(e.target.value)}/>
            </Form.Group>
            <Form.Group>
              <Form.Label>Premises</Form.Label>
              <Form.Control type="textarea" value={this.state.theoremBeingAdded.premises} onChange={e => this.updateTheoremPremises(e.target.value)}/>
            </Form.Group>
            <Form.Group>
              <Form.Label>Conclusion</Form.Label>
              <Form.Control type="text" value={this.state.theoremBeingAdded.conclusion} onChange={e => this.updateTheoremConclusion(e.target.value)}/>
            </Form.Group>
          </Form>
        </Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={this.stopAddingTheorem}>Close</Button>
          <Button variant="primary" onClick={this.saveTheorem}>Save Changes</Button>
        </Modal.Footer>
      </Modal>
    </Page>
  }
}
