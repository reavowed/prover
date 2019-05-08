import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import styled, {css}  from "styled-components";
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
  
  ${props => props.incomplete && css`
    &::before {
      content: "?";
      color: red;
      font-weight: bold;
      margin-right: 0.25em;
    }
  `}
`;

const Capitalized = styled.span`
  text-transform: capitalize;
`;

const Result = ({title, url, buttons, deleteButton, children, updateChapter, incomplete}) => {
  const deleteEntry = () => {
    updateChapter(url, {method: "DELETE"})
  };
  const moveEntry = (direction) => {
    updateChapter(url + "/move?direction=" + direction, {method: "POST"})
  };
  return <ResultWrapper>
    <FlexRow>
      <FlexRow.Grow><ResultTitle href={url} incomplete={incomplete}>{title}</ResultTitle></FlexRow.Grow>
      {buttons}
      {deleteButton && <Button size="sm" variant="danger" className="ml-1" onClick={deleteEntry}><span className="fas fa-ban"/></Button>}
      <Button size="sm" className="ml-1" onClick={() => moveEntry("up")}><span className="fas fa-arrow-up"/></Button>
      <Button size="sm" className="ml-1" onClick={() => moveEntry("down")}><span className="fas fa-arrow-down"/></Button>
    </FlexRow>
    {children}
  </ResultWrapper>
};

const InferenceResult = ({title, entry, updateChapter, incomplete}) => {
  return <Result title={<>{title}: {formatHtml(entry.name)}</>}
                 url={entry.url}
                 updateChapter={updateChapter}
                 deleteButton
                 incomplete={incomplete}>
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

class ChapterTitle extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      hovered: false,
      editing: false
    }
  }

  saveTitle = () => {
    return this.props.updateChapter(this.props.url + "/title", {method: "PUT", body: this.state.newTitle})
      .then(() => this.setState({editing: false}));
  };

  render() {
    const {title} = this.props;
    const {editing, hovered, newTitle} = this.state;
    return editing ?
      <FlexRow className="mb-2">
        <FlexRow.Grow><Form.Control type="text" value={newTitle} onChange={e => this.setState({newTitle: e.target.value})} /></FlexRow.Grow>
        <Button className="ml-1" variant="success" onClick={this.saveTitle}><i className="fas fa-check"/></Button>
        <Button className="ml-1" variant="danger" onClick={() => this.setState({editing: false})}><i className="fas fa-times"/></Button>
      </FlexRow> :
      <h3 onMouseEnter={() => this.setState({hovered: true})} onMouseLeave={() => this.setState({hovered: false})}>
        {title}
        {hovered && <Button className="ml-3" size="sm" onClick={() => this.setState({editing: true, hovered: false, newTitle: title})}><i className="fas fa-edit"/></Button>}
      </h3>;
  }

}

export class Chapter extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      title: props.title,
      url: props.url,
      entries: Parser.parseEntries(props.entries),
      theoremBeingAdded: null
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
        this.setState({
          title: newProps.title,
          url: newProps.url,
          entries: Parser.parseEntries(newProps.entries)
        });
        if (window.location.pathname !== newProps.url) {
          history.replaceState({}, "", newProps.url);
        }
      });
  };
  renderEntry = (entry) => {
    switch (entry.type) {
      case "axiom":
        return <InferenceResult key={entry.url} title="Axiom" entry={entry} updateChapter={this.updateChapter}/>;
      case "theorem":
        return <InferenceResult key={entry.url} title="Theorem" entry={entry} updateChapter={this.updateChapter} incomplete={!entry.isComplete}/>;
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
                       key={entry.url}
                       updateChapter={this.updateChapter}>
          {entry.defaultTermName} is {definition.article} {definition.name} {formatHtml(definition.componentFormatString, s => replacePlaceholders(s, entry.components))} if <ExpressionComponent expression={entry.definingStatement} boundVariableLists={[]}/>.
        </Result>;
      case "propertyDefinition":
        const typeDefinition = window.typeDefinitions[entry.parentTypeSymbol];
        return <Result title={<>Definition: <span style={{textTransform: "capitalize"}}>{entry.name} {entry.parentTypeName}</span></>}
                       url={entry.url}
                       key={entry.url}
                       updateChapter={this.updateChapter}>
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
      theoremBeingAdded: {
        name: "",
        premises: "",
        conclusion: ""
      }
    })
  };
  stopAddingTheorem = () => {
    this.setState({theoremBeingAdded: null})
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
    this.updateChapter(path.join(this.state.url, "theorems"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(theoremToAdd)
    }).then(this.stopAddingTheorem);
  };

  startAddingTerm = () => {
    this.setState({
      termBeingAdded: {
        symbol: "",
        components: "",
        name: "",
        format: "",
        premises: "",
        definition: "",
        shorthand: "",
        attributes: ""
      }
    })
  };
  stopAddingTerm = () => {
    this.setState({termBeingAdded: null})
  };
  updateTermSymbol = (newSymbol) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.symbol = newSymbol;
    this.setState({termBeingAdded});
  };
  updateTermComponents = (newComponents) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.components = newComponents;
    this.setState({termBeingAdded});
  };
  updateTermName = (newName) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.name = newName;
    this.setState({termBeingAdded});
  };
  updateTermFormat = (newFormat) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.format = newFormat;
    this.setState({termBeingAdded});
  };
  updateTermPremises = (newPremises) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.premises = Parser.replaceShorthands(newPremises);
    this.setState({termBeingAdded});
  };
  updateTermDefinition = (newDefinition) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.definition = Parser.replaceShorthands(newDefinition);
    this.setState({termBeingAdded});
  };
  updateTermShorthand = (newShorthand) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.shorthand = Parser.replaceShorthands(newShorthand);
    this.setState({termBeingAdded});
  };
  updateTermAttributes = (newAttributes) => {
    const {termBeingAdded} = this.state;
    termBeingAdded.attributes = Parser.replaceShorthands(newAttributes);
    this.setState({termBeingAdded});
  };
  saveTerm = () => {
    const {termBeingAdded} = this.state;
    const termToAdd = _.cloneDeep(termBeingAdded);
    termToAdd.premises = _.filter(termToAdd.premises.split(/\r?\n/), s => s.length);
    this.updateChapter(path.join(this.state.url, "termDefinitions"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(termToAdd)
    }).then(this.stopAddingTerm);
  };

  createUpdater = (statePropertyName, innerPropertyName, transformer) => (newPropertyValue) => {
    const currentStateValue = this.state[statePropertyName];
    currentStateValue[innerPropertyName] = transformer ? transformer(newPropertyValue) : newPropertyValue;
    this.setState({statePropertyName: currentStateValue});
  };
  startAddingProperty = () => {
    this.setState({
      propertyBeingAdded: {
        symbol: "",
        parentType: "",
        defaultTermName: "",
        parentComponentTypes: "",
        name: "",
        definition: ""
      }
    })
  };
  stopAddingProperty = () => {
    this.setState({propertyBeingAdded: null})
  };
  saveProperty = () => {
    const {propertyBeingAdded} = this.state;
    this.updateChapter(path.join(this.state.url, "propertyDefinitions"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(propertyBeingAdded)
    }).then(this.stopAddingProperty);
  };

  render() {
    const {bookLink, summary, previous, next} = this.props;
    const {title, url, entries, theoremBeingAdded, termBeingAdded, propertyBeingAdded} = this.state;

    const updateControl = (caption, statePropertyName, innerPropertyName, transformer) => {
      return <Form.Group>
        <Form.Label><strong>{caption}</strong></Form.Label>
        <Form.Control type="text" value={this.state[statePropertyName][innerPropertyName]} onChange={e => this.createUpdater(statePropertyName, innerPropertyName, transformer)(e.target.value)}/>
      </Form.Group>
    };

    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, {title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <ChapterTitle title={title} url={url} updateChapter={this.updateChapter} />
      <p>{summary}</p>
      {entries.map(this.renderEntry)}
      <hr/>
      {!theoremBeingAdded && !termBeingAdded && !propertyBeingAdded && <>
        <Button onClick={this.startAddingTheorem}>Add theorem</Button>
        <Button className="ml-2" onClick={this.startAddingTerm}>Add term</Button>
        <Button className="ml-2" onClick={this.startAddingProperty}>Add property</Button>
      </>}
      {theoremBeingAdded && <>
        <h4>Add theorem</h4>
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
        <Button variant="primary" onClick={this.saveTheorem}>Save theorem</Button>
      </>}
      {termBeingAdded && <>
        <h4>Add term</h4>
        <Form.Group>
          <Form.Label><strong>Symbol</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.symbol}
                        onChange={(e) => this.updateTermSymbol(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Components</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.components}
                        onChange={(e) => this.updateTermComponents(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Name</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.name}
                        onChange={(e) => this.updateTermName(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Format</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.format}
                        onChange={(e) => this.updateTermFormat(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Premises</strong></Form.Label>
          <Form.Control as="textarea"
                        value={this.state.termBeingAdded.premises}
                        onChange={(e) => this.updateTermPremises(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Definition</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.definition}
                        onChange={(e) => this.updateTermDefinition(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Shorthand</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.shorthand}
                        onChange={(e) => this.updateTermShorthand(e.target.value)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Attributes</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.attributes}
                        onChange={(e) => this.updateTermAttributes(e.target.value)}/>
        </Form.Group>
        <Button size="sm" onClick={this.saveTerm}>Save term</Button>
        <Button size="sm" className="ml-2" variant="danger" onClick={this.stopAddingTerm}>Cancel</Button>
      </>}
      {propertyBeingAdded && <>
        <h4>Add property</h4>
        {updateControl("Symbol", "propertyBeingAdded", "symbol")}
        {updateControl("Parent type", "propertyBeingAdded", "parentType")}
        {updateControl("Default term name", "propertyBeingAdded", "defaultTermName")}
        {updateControl("Parent component names", "propertyBeingAdded", "parentComponentTypes")}
        {updateControl("Explicit name (if different from symbol)", "propertyBeingAdded", "name")}
        {updateControl("Definition", "propertyBeingAdded", "definition", Parser.replaceShorthands)}
        <Button size="sm" onClick={this.saveProperty}>Save term</Button>
        <Button size="sm" className="ml-2" variant="danger" onClick={this.stopAddingProperty}>Cancel</Button>
      </>}
    </Page>
  }
}
