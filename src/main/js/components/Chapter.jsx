import path from "path";
import React from "react";
import {DragDropContext, Draggable, Droppable} from "react-beautiful-dnd";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import styled, {css}  from "styled-components";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {CopiableExpression} from "./ExpressionComponent";
import {formatHtml, replacePlaceholders} from "./helpers/Formatter";
import {FlexRow} from "./FlexRow";
import {InlineTextEditor} from "./helpers/InlineTextEditor";
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
  return <ResultWrapper>
    <FlexRow>
      <FlexRow.Grow><ResultTitle href={url} incomplete={incomplete}>{title}</ResultTitle></FlexRow.Grow>
      {buttons}
      {deleteButton && <Button size="sm" variant="danger" className="ml-1 py-0" onClick={deleteEntry}><span className="fas fa-ban"/></Button>}
    </FlexRow>
    {children}
  </ResultWrapper>
};

const InferenceResult = ({title, entry, updateChapter, incomplete, editing}) => {
  return <Result title={<>{title}: {formatHtml(entry.name)}</>}
                 url={entry.url}
                 updateChapter={updateChapter}
                 deleteButton={editing}
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
    const {title, entry, children, updateChapter, editing} = this.props;

    return <>
      <Result title={<>{title}: <CopiableExpression expression={entry.defaultValue} boundVariableLists={[]}/></>}
              url={entry.url}
              updateChapter={updateChapter}
              buttons={editing && <Button size="sm" variant="primary" className="ml-1 mb-n2" onClick={this.startEditingShorthand}>Shorthand</Button>}
      >
        {children}
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
      title: props.title,
      url: props.url,
      entries: Parser.parseEntries(props.entries),
      theoremBeingAdded: null,
      editing: false
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
        window.definitions = newProps.definitions;
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

  renderEntry = (entry, editing) => {
    switch (entry.type) {
      case "axiom":
        return <InferenceResult key={entry.url} title="Axiom" entry={entry} updateChapter={this.updateChapter} editing={editing}/>;
      case "theorem":
        return <InferenceResult key={entry.url} title="Theorem" entry={entry} updateChapter={this.updateChapter} incomplete={!entry.isComplete} editing={editing} />;
      case "statementDefinition":
        return <DefinitionResult key={entry.url} title="Statement Definition" entry={entry} updateChapter={this.updateChapter} editing={editing}>
          {entry.definingStatement && <><CopiableExpression expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <CopiableExpression expression={entry.definingStatement} boundVariableLists={[]}/>.</>}
        </DefinitionResult>;
      case "termDefinition":
        return <DefinitionResult key={entry.url} title="Term Definition" entry={entry} updateChapter={this.updateChapter} editing={editing}>
          <ResultWithPremises premises={entry.premises}
                              result={<><CopiableExpression expression={entry.defaultValue} boundVariableLists={[]}/> is defined by <CopiableExpression expression={entry.definingStatement} boundVariableLists={[]}/></>}/>
        </DefinitionResult>;
      case "typeDefinition":
        const definition = window.typeDefinitions[entry.symbol];
        return <Result title={<>Definition: <Capitalized>{definition.name}</Capitalized></>}
                       url={entry.url}
                       key={entry.url}
                       updateChapter={this.updateChapter}>
          {entry.defaultTermName} is {definition.article} {definition.name} {formatHtml(definition.componentFormatString, s => replacePlaceholders(s, entry.components))} if <CopiableExpression expression={entry.definingStatement} boundVariableLists={[]}/>.
        </Result>;
      case "propertyDefinition":
        const typeDefinition = window.typeDefinitions[entry.parentTypeSymbol];
        return <Result title={<>Definition: <span style={{textTransform: "capitalize"}}>{entry.name} {typeDefinition.name}</span></>}
                       url={entry.url}
                       key={entry.url}
                       updateChapter={this.updateChapter}>
          <Capitalized>{typeDefinition.article}</Capitalized> {typeDefinition.name} {entry.defaultTermName} {formatHtml(typeDefinition.componentFormatString, s => replacePlaceholders(s, entry.parentTypeComponents))} is {entry.name} if <CopiableExpression expression={entry.definingStatement} boundVariableLists={[]}/>.
        </Result>;
      case "comment":
        return <p key={entry.url}>{entry.text}</p>;
      case "placeholder":
        return <React.Fragment key={entry.url}/>;
      default:
        throw `Unrecognised entry '${entry.type}'`;
    }
  };
  renderDraggableEntry = (entry, index, editing) => {
    const key = entry.url;
    return <Draggable draggableId={key} index={index} key={key} isDragDisabled={!editing || this.state.disableDrag}>
      {(provided) => (
        <div ref={provided.innerRef}{...provided.draggableProps}{...provided.dragHandleProps}>
          {this.renderEntry(entry, editing)}
        </div>
      )}
    </Draggable>
  };
  onDragEnd = (result) => {
    if (!result.destination) {
      return;
    }
    const reorder = (list, startIndex, endIndex) => {
      const result = Array.from(list);
      const [removed] = result.splice(startIndex, 1);
      result.splice(endIndex, 0, removed);
      return result;
    };
    const entryToMove = this.state.entries[result.source.index];
    const temporaryEntries = reorder(this.state.entries, result.source.index, result.destination.index);
    this.setState(
      {temporaryEntries, disableDrag: true},
      () => {
        this.updateChapter(path.join(entryToMove.url, "index"), {
          method: "PUT",
          headers: {"Content-Type": "application/json"},
          body: result.destination.index
        }).catch(() => {})
          .then(() => this.setState({temporaryEntries: null, disableDrag: false}));
      });
  };

  updateTitle = (newTitle) => {
    return this.updateChapter(this.state.url + "/title", {method: "PUT", body: newTitle});
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
  updateTheoremPremises = (event) => {
    const [premises, callback] = Parser.replaceShorthands(event);
    const theoremBeingAdded = _.assign({}, this.state.theoremBeingAdded, {premises});
    this.setState({theoremBeingAdded}, callback)
  };
  updateTheoremConclusion = (event) => {
    const [conclusion, callback] = Parser.replaceShorthands(event);
    const theoremBeingAdded = _.assign({}, this.state.theoremBeingAdded, {conclusion});
    this.setState({theoremBeingAdded}, callback);
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
  updateTermPremises = (event) => {
    const [premises, callback] = Parser.replaceShorthands(event);
    const termBeingAdded = _.assign({}, this.state.termBeingAdded, {premises});
    this.setState({termBeingAdded}, callback);
  };
  updateTermDefinition = (event) => {
    const [definition, callback] = Parser.replaceShorthands(event);
    const termBeingAdded = _.assign({}, this.state.termBeingAdded, {definition});
    this.setState({termBeingAdded}, callback);
  };
  updateTermShorthand = (event) => {
    const [shorthand, callback] = Parser.replaceShorthands(event);
    const termBeingAdded = _.assign({}, this.state.termBeingAdded, {shorthand});
    this.setState({termBeingAdded}, callback);
  };
  updateTermAttributes = (event) => {
    const [attributes, callback] = Parser.replaceShorthands(event);
    const termBeingAdded = _.assign({}, this.state.termBeingAdded, {attributes});
    this.setState({termBeingAdded}, callback);
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

  createUpdater = (statePropertyName, innerPropertyName, transformer) => (event) => {
    const [newValue, callback] = transformer ? transformer(event) : [event.target.value, null];
    const newStateValue = _.assign({}, this.state.statePropertyName, {innerPropertyName: newValue});
    this.setState({statePropertyName: newStateValue}, callback);
  };

  startAddingProperty = () => {
    this.setState({
      propertyBeingAdded: {
        symbol: "",
        parentType: "",
        defaultTermName: "",
        parentComponents: "",
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

  startAddingType = () => {
    this.setState({
      typeBeingAdded: {
        symbol: "",
        defaultTermName: "",
        otherComponents: "",
        format: "",
        name: "",
        definition: ""
      }
    })
  };
  stopAddingType = () => {
    this.setState({typeBeingAdded: null})
  };
  saveType = () => {
    const {typeBeingAdded} = this.state;
    this.updateChapter(path.join(this.state.url, "typeDefinitions"), {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(typeBeingAdded)
    }).then(this.stopAddingProperty);
  };

  render() {
    const {bookLink, summary, previous, next} = this.props;
    const {title, url, entries, temporaryEntries, theoremBeingAdded, termBeingAdded, typeBeingAdded, propertyBeingAdded, editing} = this.state;

    const updateControl = (caption, statePropertyName, innerPropertyName, transformer) => {
      return <Form.Group>
        <Form.Label><strong>{caption}</strong></Form.Label>
        <Form.Control type="text" value={this.state[statePropertyName][innerPropertyName]} onChange={e => this.createUpdater(statePropertyName, innerPropertyName, transformer)(e)}/>
      </Form.Group>
    };

    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, {title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <Button className="ml-3 float-right" size="sm" onClick={() => this.setState({editing: !editing})}><i className={"fas fa-" + (editing ? "times" : "edit")}/></Button>
      <h3><InlineTextEditor text={title} callback={this.updateTitle}/></h3>
      <p>{summary}</p>
      <DragDropContext onDragEnd={this.onDragEnd}>
        <Droppable droppableId="entries">
          {(provided) =>
            <div ref={provided.innerRef} {...provided.droppableProps}>
              {(temporaryEntries || entries).map((entry, index) => this.renderDraggableEntry(entry, index, editing))}
              {provided.placeholder}
            </div>
          }
        </Droppable>
      </DragDropContext>
      <hr/>
      {!theoremBeingAdded && !termBeingAdded && !typeBeingAdded && !propertyBeingAdded && <>
        <Button onClick={this.startAddingTheorem}>Add theorem</Button>
        <Button className="ml-2" onClick={this.startAddingTerm}>Add term</Button>
        <Button className="ml-2" onClick={this.startAddingType}>Add type</Button>
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
          <Form.Control as="textarea" value={this.state.theoremBeingAdded.premises} onChange={e => this.updateTheoremPremises(e)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label>Conclusion</Form.Label>
          <Form.Control type="text" value={this.state.theoremBeingAdded.conclusion} onChange={e => this.updateTheoremConclusion(e)}/>
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
                        onChange={(e) => this.updateTermPremises(e)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Definition (use "_" as placeholder)</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.definition}
                        onChange={(e) => this.updateTermDefinition(e)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Shorthand</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.shorthand}
                        onChange={(e) => this.updateTermShorthand(e)}/>
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Attributes</strong></Form.Label>
          <Form.Control type="text"
                        value={this.state.termBeingAdded.attributes}
                        onChange={(e) => this.updateTermAttributes(e)}/>
        </Form.Group>
        <Button size="sm" onClick={this.saveTerm}>Save term</Button>
        <Button size="sm" className="ml-2" variant="danger" onClick={this.stopAddingTerm}>Cancel</Button>
      </>}
      {typeBeingAdded && <>
        <h4>Add type</h4>
        {updateControl("Symbol", "typeBeingAdded", "symbol")}
        {updateControl("Default term name", "typeBeingAdded", "defaultTermName")}
        {updateControl("Other components", "typeBeingAdded", "otherComponents")}
        {updateControl("Format", "typeBeingAdded", "format")}
        {updateControl("Explicit name (if different from symbol)", "typeBeingAdded", "name")}
        {updateControl("Definition", "typeBeingAdded", "definition", Parser.replaceShorthands)}
        <Button size="sm" onClick={this.saveType}>Save type</Button>
        <Button size="sm" className="ml-2" variant="danger" onClick={this.stopAddingType}>Cancel</Button>
      </>}
      {propertyBeingAdded && <>
        <h4>Add property</h4>
        {updateControl("Symbol", "propertyBeingAdded", "symbol")}
        {updateControl("Parent type", "propertyBeingAdded", "parentType")}
        {updateControl("Default term name", "propertyBeingAdded", "defaultTermName")}
        {updateControl("Parent components", "propertyBeingAdded", "parentComponents")}
        {updateControl("Explicit name (if different from symbol)", "propertyBeingAdded", "name")}
        {updateControl("Definition", "propertyBeingAdded", "definition", Parser.replaceShorthands)}
        <Button size="sm" onClick={this.saveProperty}>Save property</Button>
        <Button size="sm" className="ml-2" variant="danger" onClick={this.stopAddingProperty}>Cancel</Button>
      </>}
    </Page>
  }
}
