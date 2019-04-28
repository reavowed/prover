import * as path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import Row from "react-bootstrap/Row";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs";
import {ExpressionComponent} from "./ExpressionComponent";
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
      newSymbol: definition.symbol
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
  render() {
    const {url, bookLink, chapterLink, previous, next, usages} = this.props;
    const {definition, newSymbol} = this.state;
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>Term Definition:  <ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/></h3>
      <ResultWithPremises premises={definition.premises}
                          result={<><ExpressionComponent expression={definition.defaultValue} boundVariableLists={[]}/> is defined by <ExpressionComponent expression={definition.definingStatement} boundVariableLists={[]}/></>}/>
      <hr/>
      <Row>
        <Col xs={1}><strong>Symbol</strong></Col>
        <Col xs={10}><Form.Control type="text" value={newSymbol} onChange={e => this.setState({newSymbol: e.target.value})} /></Col>
        <Col xs={1}><Button variant="success" size="sm" onClick={this.setSymbol}><i className="fas fa-check"/></Button></Col>
      </Row>
      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>;
  }
}
