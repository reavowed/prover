import * as path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import Row from "react-bootstrap/Row";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {CopiableExpression} from "./ExpressionComponent";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";
import {ResultWithPremises} from "./ResultWithPremises";
import {Usages} from "./Usages";

export class TermDefinition extends React.Component {
  constructor(props) {
    super(props);
    const definition = Parser.parseTermDefinition(props.definition);
    this.state = {
      definition: definition,
      newSymbol: definition.symbol,
      newAttributes: definition.attributes
    }
  }
  setSymbol = () => {
    return window.fetch(path.join(this.props.url, "symbol"), {method: "PUT", body: this.state.newSymbol})
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      })
      .then(() => window.location.reload());
  };
  setAttributes = () => {
    return window.fetch(path.join(this.props.url, "attributes"), {method: "PUT", body: JSON.stringify(this.state.newAttributes), headers: {"Content-Type": "application/json"}})
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      })
      .then(() => window.location.reload());
  };
  render() {
    const {url, bookLink, chapterLink, previous, next, usages} = this.props;
    const {definition, newSymbol, newAttributes} = this.state;
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
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
    </Page>;
  }
}
