import React from "react";
import Container from "react-bootstrap/Container";
import {Header} from "./Header";

export class Page extends React.Component {
  render() {
    const {breadcrumbs, children} = this.props;
    return <>
      <Header>
        {breadcrumbs}
      </Header>
      <Container>
        {children}
      </Container>
    </>
  }
}