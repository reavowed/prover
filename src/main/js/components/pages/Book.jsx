import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import {DndProvider} from "react-dnd";
import Backend from "react-dnd-html5-backend";
import {fetchJson} from "../../utils";
import {SimpleDraggableList} from "../draggableList/SimpleDraggableList";
import FlexRow from "../FlexRow";
import BookContext from "./book/BookContext";
import {ChapterAdder} from "./book/ChapterAdder";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";

function Chapter({chapter, deleteChapter, editing}) {
  return <React.Fragment key={chapter.url}>
    <FlexRow className="mt-3">
      <FlexRow.Grow>
        <h4><a href={chapter.url}>{chapter.title}</a></h4>
      </FlexRow.Grow>
      {editing && <Button size="sm" variant="danger" className="ml-1 py-0" onClick={() => deleteChapter(chapter)}><span className="fas fa-ban"/></Button>}
    </FlexRow>
    <p>{chapter.summary}</p>
  </React.Fragment>
}

export class Book extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      chapters: this.props.chapters,
      addingNewChapter: false,
      newChapter: {},
      editing: false
    }
  }

  onKeyDown = (event) => {
    if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) {
      return;
    }
    if (event.key === "e") {
      this.setState({editing: !this.state.editing});
    }
  };

  componentDidMount() {
    document.body.addEventListener('keydown', this.onKeyDown);
  }
  componentWillUnmount() {
    document.body.removeEventListener('keydown', this.onKeyDown);
  }

  updateChapters = (chapters) => {
    return this.setStatePromise({chapters});
  };

  deleteChapter = (chapter) => {
    return fetchJson(chapter.url, {method: "DELETE"})
      .then(({chapters}) => this.updateChapters(chapters));
  };

  onDropChapter = ({url, index}, target, after) => {
    if (target) {
      const newIndex = target.index + (after ? 1 : 0) - (index < target.index ? 1 : 0);
      if (newIndex !== index) {
        return fetchJson(path.join(url, "index"), {method: "PUT", body: newIndex})
          .then(({chapters}) => this.updateChapters(chapters));
      }
    }
    return Promise.resolve();
  };

  render() {
    const {title, url, previous, next} = this.props;
    const {chapters, editing} = this.state;
    const context = {
      url,
      updateChapters: this.updateChapters
    };
    return <BookContext.Provider value={context}>
      <Page breadcrumbs={<Breadcrumbs links={[{title, url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{title}</h3>
        <DndProvider backend={Backend}>
          <SimpleDraggableList
            type="ChapterEntry"
            enabled={editing}
            onDrop={this.onDropChapter}
            entries={chapters.map((chapter, index) => {return {key: chapter.url, data: {url: chapter.url, index}, element: <Chapter chapter={chapter} deleteChapter={this.deleteChapter} editing={editing} />}})}
          />
        </DndProvider>
        <hr/>
        <ChapterAdder/>
      </Page>
    </BookContext.Provider>;
  }
}
