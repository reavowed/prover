import React from "react";
import Container from "react-bootstrap/Container";
import Navbar from "react-bootstrap/Navbar";
import styled from "styled-components";

const NavbarWrapper = styled.div`
  background-color: #343a40;
  & .navbar {
    padding-left: 0;
    padding-right: 0;
  }
`;

class Header extends React.Component {
  render() {
    const {children} = this.props;
    return <>
      <NavbarWrapper>
        <Container>
          <Navbar bg="dark" variant="dark" expand>
            <Navbar.Brand href="/books">Prover</Navbar.Brand>
          </Navbar>
        </Container>
      </NavbarWrapper>
      {children}
    </>
  }
}

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
          <Container className="pb-4" style={{backgroundColor: "white", flex: "1 1 auto"}}>
            {children}
          </Container>
        </div>
      </div>
    </>
  }
}
