import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";

export class Book extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      chapters: this.props.chapters,
      addingNewChapter: false,
      newChapter: {}
    }
  }

  openNewChapterModal = () => {
    this.setState({
      addingNewChapter: true,
      newChapter: {
        title: '',
        summary: ''
      }
    })
  };
  hideNewChapterModal = () => {
    this.setState({addingNewChapter: false})
  };
  updateNewChapterTitle = (e) => {
    const newChapter = this.state.newChapter;
    newChapter.title = e.target.value;
    this.setState({newChapter});
  };
  updateNewChapterSummary = (e) => {
    const newChapter = this.state.newChapter;
    newChapter.summary = e.target.value;
    this.setState({newChapter});
  };
  addNewChapter = () => {
    window.fetchJson(
      path.join(this.props.url, "chapters"),
      {method: "POST", body: this.state.newChapter}
    ).then(({chapters}) => {
      this.setState({chapters});
      this.hideNewChapterModal();
    });
  };

  render() {
    const {title, url, previous, next} = this.props;
    const {chapters} = this.state;
    const newChapterModal = <Modal show={this.state.addingNewChapter} onHide={this.hideNewChapterModal}>
      <Modal.Header closeButton><Modal.Title>New chapter</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group>
            <Form.Label>Title</Form.Label>
            <Form.Control type="text" value={this.state.newChapter.title} onChange={this.updateNewChapterTitle}/>
          </Form.Group>
          <Form.Group>
            <Form.Label>Summary</Form.Label>
            <Form.Control type="text" value={this.state.newChapter.summary} onChange={this.updateNewChapterSummary}/>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={this.hideNewChapterModal}>Close</Button>
        <Button variant="primary" onClick={this.addNewChapter}>Save Changes</Button>
      </Modal.Footer>
    </Modal>;
    return <Page breadcrumbs={<Breadcrumbs links={[{title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}</h3>
      {chapters.map(chapter =>
        <React.Fragment key={chapter.url}>
          <h4 className="mt-3"><a href={chapter.url}>{chapter.title}</a></h4>
          <p>{chapter.summary}</p>
        </React.Fragment>
      )}
      <Button className="mt-3 float-right" onClick={this.openNewChapterModal}>New chapter</Button>
      {newChapterModal}
    </Page>
  }
}
