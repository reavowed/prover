import * as path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import Row from "react-bootstrap/Row";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {CopiableExpression} from "../ExpressionComponent";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";
import {ResultWithPremises} from "../ResultWithPremises";
import {Usages} from "./components/Usages";

export class TermDefinition extends React.Component {
  constructor(props) {
    super(props);
    const definition = this.getParser().parseTermDefinition(props.definition);
    this.state = {
      definition: definition,
      newSymbol: definition.symbol,
      newAttributes: definition.attributes
    }
  }
  getParser = () => new Parser(this.props.definitions, this.props.typeDefinitions);
  setSymbol = () => {
    return window.fetchJson(path.join(this.props.url, "symbol"), {method: "PUT", body: this.state.newSymbol});
  };
  setAttributes = () => {
    return window.fetchJson(path.join(this.props.url, "attributes"), {method: "PUT", body: this.state.newAttributes});
  };
  render() {
    const {url, bookLink, chapterLink, previous, next, usages, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations} = this.props;
    const {definition, newSymbol, newAttributes} = this.state;
    const entryContext = {parser: this.getParser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};
    return <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>Term Definition:  <CopiableExpression expression={definition.defaultValue} /></h3>
        <ResultWithPremises premises={definition.premises}
                            result={<><CopiableExpression expression={definition.defaultValue} /> is defined by <CopiableExpression expression={definition.definingStatement} /></>}/>
        <hr/>
        <Row>
          <Col xs={1}><strong>Symbol</strong></Col>
          <Col xs={10}><Form.Control type="text" value={newSymbol} onChange={e => this.setState({newSymbol: e.target.value})} /></Col>
          <Col xs={1}><Button variant="success" size="sm" onClick={this.setSymbol}><i className="fas fa-check"/></Button></Col>
        </Row>
        <Row>
          <Col xs={1}><strong>Attributes</strong></Col>
          <Col xs={10}><Form.Control type="text" value={newAttributes.join(" ")} onChange={e => this.setState({newAttributes: e.target.value.split(" ")})} /></Col>
          <Col xs={1}><Button variant="success" size="sm" onClick={this.setAttributes}><i className="fas fa-check"/></Button></Col>
        </Row>
        {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
      </Page>
    </EntryContext.Provider>;
  }
}