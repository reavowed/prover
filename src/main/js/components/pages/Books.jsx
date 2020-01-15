import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {Page} from "./Page";

export class Books extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      books: this.props.books,
      addingNewBook: false,
      newBook: {}
    }
  }

  openNewBookModal = () => {
    this.setState({
      addingNewBook: true,
      newBook: {
        title: '',
        imports: []
      }
    })
  };
  hideNewBookModal = () => {
    this.setState({addingNewBook: false})
  };
  updateNewBookTitle = (e) => {
    const newBook = this.state.newBook;
    newBook.title = e.target.value;
    this.setState({newBook});
  };
  updateNewBookImports = (e) => {
    const newBook = this.state.newBook;
    newBook.imports = _.chain(e.target).filter("selected").map(o => o.value).value();
    this.setState({newBook});
  };
  addNewBook = () => {
    window.fetchJson("/books", {method: "POST", body: this.state.newBook})
      .then(({books}) => {
        this.setState({books});
        this.hideNewBookModal();
      });
  };

  render() {
    const {books} = this.state;
    const newBookModal = <Modal show={this.state.addingNewBook} onHide={this.hideNewBookModal}>
      <Modal.Header closeButton><Modal.Title>New book</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group>
            <Form.Label>Title</Form.Label>
            <Form.Control type="text" value={this.state.newBook.title} onChange={this.updateNewBookTitle}/>
          </Form.Group>
          <Form.Group>
            <Form.Label>Imports</Form.Label>
            <Form.Control as="select" multiple onChange={this.updateNewBookImports}>
              {books.map(book => <option key={book.url} selected={_.includes(this.state.newBookImports, book.title)}>{book.title}</option>)}
            </Form.Control>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={this.hideNewBookModal}>Close</Button>
        <Button variant="primary" onClick={this.addNewBook}>Save Changes</Button>
      </Modal.Footer>
    </Modal>;

    return <Page>
      {books.map(book =>
        <h3 className="mt-3" key={book.url}>
          <a href={book.url} key={book.url}>{book.title}</a>
        </h3>
      )}
      <Button className="mt-3 float-right" onClick={this.openNewBookModal}>New book</Button>
      {newBookModal}
    </Page>
  }
}
