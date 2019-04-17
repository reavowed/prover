import React from "react";
import Breadcrumb from "react-bootstrap/Breadcrumb";
import Container from "react-bootstrap/Container";
import styled from "styled-components";

const BreadcrumbWrapper = styled.div`
  background-color: #e9ecef;
  & .breadcrumb {
    padding-left: 0;
    padding-right: 0;
    margin-bottom: 0;
  }
`;

export const Breadcrumbs = ({links}) =>
  <BreadcrumbWrapper>
    <Container>
      <Breadcrumb>
        <Breadcrumb.Item href="/books">Books</Breadcrumb.Item>
        {links.slice(0, links.length - 1).map(({title, url}) => <Breadcrumb.Item key={url} href={url}>{title}</Breadcrumb.Item>)}
        <Breadcrumb.Item active>{links[links.length - 1].title}</Breadcrumb.Item>
      </Breadcrumb>
    </Container>
  </BreadcrumbWrapper>;
