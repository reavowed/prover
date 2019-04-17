import React from "react";
import Container from "react-bootstrap/Container";
import {Header} from "./Header";


export class Page extends React.Component {
  render() {
    const {breadcrumbs, children} = this.props;
    return <>
      <div style={{display: "flex", flexFlow: "column", height: "100%"}}>
        <div  style={{flex: "0 1 auto"}}>
          <Header>
            {breadcrumbs}
          </Header>
        </div>
        <div style={{backgroundColor: "ghostwhite", flex: "1 1 auto", display: "flex", flexFlow: "column"}}>
          <Container style={{backgroundColor: "white", flex: "1 1 auto"}}>
            {children}
          </Container>
        </div>
      </div>
    </>
  }
}
