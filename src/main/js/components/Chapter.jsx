import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import styled from "styled-components";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {ExpressionComponent} from "./ExpressionComponent";
import {formatHtml, replacePlaceholders} from "./helpers/Formatter";
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

const Capitalized = styled.span`
  text-transform: capitalize;
`;

const Result = ({title, url, buttons, deleteButton, children, updateChapter}) => {
  const deleteEntry = () => {
    updateChapter(url, {method: "DELETE"})
  };
  const moveEntry = (direction) => {
    updateChapter(url + "/move?direction=" + direction, {method: "POST"})
  };
  return <ResultWrapper>
    <FlexRow>
      <FlexRow.Grow><ResultTitle href={url}>{title}</ResultTitle></FlexRow.Grow>
      {buttons}
      {deleteButton && <Button size="sm" variant="danger" className="ml-1" onClick={deleteEntry}><span className="fas fa-ban"/></Button>}
      <Button size="sm" className="ml-1" onClick={() => moveEntry("up")}><span className="fas fa-arrow-up"/></Button>
      <Button size="sm" className="ml-1" onClick={() => moveEntry("down")}><span className="fas fa-arrow-down"/></Button>
    </FlexRow>
    {children}
  </ResultWrapper>
};

const InferenceResult = ({title, entry, updateChapter}) => {
  return <Result title={<>{title}: {entry.name}</>}
                 url={entry.url}
                 updateChapter={updateChapter}
                 deleteButton>
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
      .updateChapter(this.props.entry.url + "/shorthand", {
        method: "PUT",
        body: this.state.shorthand
      })
      .then(() => this.stopEditingShorthand());
  };
  render() {
    const {title, entry, children, updateChapter} = this.props;

    return <>
      <Result title={<>{title}: <ExpressionComponent expression={entry.defaultValue} boundVariableLists={[]}/></>}
              url={entry.url}
              updateChapter={updateChapter}
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
}

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
    return window.fetch(fetchUrl, fetchData)
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
        return <InferenceResult key={entry.url} title="Axiom" entry={entry} updateChapter={this.updateChapter}/>;
      case "theorem":
        return <InferenceResult key={entry.url} title="Theorem" entry={entry} updateChapter={this.updateChapter}/>;
      case "statementDefinition":
        return <DefinitionResult key={entry.url} title="Statement Definition" entry={entry} updateChapter={this.updateChapter}>
          {entry.definingStatement && <><ExpressionComponent expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <ExpressionComponent expression={entry.definingStatement} boundVariableLists={[]}/>.</>}
        </DefinitionResult>;
      case "termDefinition":
        return <DefinitionResult key={entry.url} title="Term Definition" entry={entry} updateChapter={this.updateChapter}>
          <ResultWithPremises premises={entry.premises}
                              result={<><ExpressionComponent expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <ExpressionComponent expression={entry.definingStatement} boundVariableLists={[]}/></>}/>
        </DefinitionResult>;
      case "typeDefinition":
        const definition = window.typeDefinitions[entry.symbol];
        return <Result title={<>Definition: <Capitalized>{definition.name}</Capitalized></>}
                       url={entry.url}
                       key={entry.url}>
          {entry.defaultTermName} is {definition.article} {definition.name} {formatHtml(definition.componentFormatString, s => replacePlaceholders(s, entry.components))} if <ExpressionComponent expression={entry.definingStatement} boundVariableLists={[]}/>.
        </Result>;
      case "propertyDefinition":
        const typeDefinition = window.typeDefinitions[entry.parentTypeSymbol];
        return <Result title={<>Definition: <span style={{textTransform: "capitalize"}}>{entry.name} {entry.parentTypeName}</span></>}
                       url={entry.url}
                       key={entry.url}>
          <Capitalized>{typeDefinition.article}</Capitalized> {typeDefinition.name} {entry.defaultTermName} {formatHtml(typeDefinition.componentFormatString, s => replacePlaceholders(s, entry.parentTypeComponents))} is {entry.name} if <ExpressionComponent expression={entry.definingStatement} boundVariableLists={[]}/>.
        </Result>;
      case "comment":
        return <p key={entry.key}>{entry.text}</p>;
      default:
        return <React.Fragment key={entry.key}/>;
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
    theoremBeingAdded.premises = Parser.replaceShorthands(newPremises);
    this.setState({theoremBeingAdded});
  };
  updateTheoremConclusion = (newConclusion) => {
    const {theoremBeingAdded} = this.state;
    theoremBeingAdded.conclusion = Parser.replaceShorthands(newConclusion);
    this.setState({theoremBeingAdded});
  };
  saveTheorem = () => {
    const {theoremBeingAdded} = this.state;
    const theoremToAdd = _.cloneDeep(theoremBeingAdded);
    theoremToAdd.premises = _.filter(theoremToAdd.premises.split(/\r?\n/), s => s.length);
    this.updateChapter(path.join(this.props.url, "theorems"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(theoremToAdd)
    }).then(this.stopAddingTheorem);
  };

  render() {
    const {title, url, bookLink, summary, previous, next} = this.props;
    const {entries} = this.state;
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, {title, url}]}/>}>
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
              <Form.Control as="textarea" value={this.state.theoremBeingAdded.premises} onChange={e => this.updateTheoremPremises(e.target.value)}/>
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
