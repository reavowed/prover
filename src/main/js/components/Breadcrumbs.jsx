import React from "react";
import Breadcrumb from "react-bootstrap/Breadcrumb";
import Container from "react-bootstrap/Container";
import styled from "styled-components";

const BreadcrumbWrapper = styled.div`
  background-color: #e9ecef;
`;

class Entry extends React.Component {
  render() {
    const {entryKey} = this.props;
    return <BreadcrumbWrapper>
      <Container>
        <Breadcrumb>
          <Breadcrumb.Item href={entryKey.chapterKey.bookKey.url}>{entryKey.chapterKey.bookKey.name}</Breadcrumb.Item>
          <Breadcrumb.Item href={entryKey.chapterKey.url}>{entryKey.chapterKey.name}</Breadcrumb.Item>
          <Breadcrumb.Item active>{entryKey.name}</Breadcrumb.Item>
        </Breadcrumb>
      </Container>
    </BreadcrumbWrapper>
  }
}

export const Breadcrumbs = {Entry};