import React from "react";
import Breadcrumb from "react-bootstrap/Breadcrumb";
import Container from "react-bootstrap/Container";
import styled from "styled-components";

const BreadcrumbWrapper = styled.div`
  background-color: #e9ecef;
  & .breadcrumb {
    padding-left: 0;
    padding-right: 0;
  }
`;

function createBreadcrumbs(...keys) {
  return <BreadcrumbWrapper>
    <Container>
      <Breadcrumb>
        <Breadcrumb.Item href="/books">Books</Breadcrumb.Item>
        {keys.slice(0, keys.length - 1).map(key => <Breadcrumb.Item key={key.url}
                                                                    href={key.url}>{key.name}</Breadcrumb.Item>)}
        <Breadcrumb.Item active>{keys[keys.length - 1].name}</Breadcrumb.Item>
      </Breadcrumb>
    </Container>
  </BreadcrumbWrapper>
}

const Book = ({bookKey}) => createBreadcrumbs(bookKey);
const Chapter = ({chapterKey}) => createBreadcrumbs(chapterKey.bookKey, chapterKey);
const Entry = ({entryKey}) => createBreadcrumbs(entryKey.chapterKey.bookKey, entryKey.chapterKey, entryKey);

export const Breadcrumbs = {Book, Chapter, Entry};
