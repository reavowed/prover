import React from "react";
import Container from "react-bootstrap/Container";
import Navbar from "react-bootstrap/Navbar";
import styled from "styled-components";

const NavbarWrapper = styled.div`
  background-color: #343a40;
`;

export class Header extends React.Component {
  render() {
    const {children} = this.props;
    return <>
      <NavbarWrapper>
        <Container>
          <Navbar bg="dark" variant="dark" expand>
            <Navbar.Brand href="/books">Prover</Navbar.Brand>
          </Navbar>
        </Container>
        {children}
      </NavbarWrapper>
    </>
  }
}