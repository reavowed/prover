import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import {DndProvider} from 'react-dnd'
import Backend from 'react-dnd-html5-backend'
import {Parser} from "../../Parser";
import {Breadcrumbs} from "./components/Breadcrumbs";
import ChapterContext from "./chapter/ChapterContext";
import ChapterEntry from "./chapter/ChapterEntry";
import ChapterEntryAdder from "./chapter/ChapterEntryAdder";
import DraggableList from "../DraggableList";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import {NavLinks} from "./components/NavLinks";
import {Page} from "./Page";
import EntryContext from "../EntryContext";

export class Chapter extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      title: props.title,
      url: props.url,
      entries: this.getParser().parseEntries(props.entries),
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

  getParser = () => new Parser(this.props.definitions, this.props.typeDefinitions);

  updateChapter = (fetchUrl, fetchData) => {
    return window.fetchJson(fetchUrl, fetchData)
      .then(newProps => {
        this.setState({
          title: newProps.title,
          url: newProps.url,
          entries: this.getParser().parseEntries(newProps.entries)
        });
        if (window.location.pathname !== newProps.url) {
          history.replaceState({}, "", newProps.url);
        }
      });
  };

  onDropEntry = ({url, index}, {index: targetIndex}, after) => {
    return this.updateChapter(path.join(url, "index"), {
      method: "PUT",
      body: targetIndex + (after ? 1 : 0) - (index < targetIndex ? 1 : 0)
    });
  };

  updateTitle = (newTitle) => {
    return this.updateChapter(this.state.url + "/title", {method: "PUT", body: newTitle});
  };

  render() {
    const {bookLink, summary, previous, next, displayShorthands, definitionShorthands, typeDefinitions} = this.props;
    const {title, url, entries, editing} = this.state;

    const context = {updateChapter: this.updateChapter, url, editing};
    const entryContext = {parser: this.getParser(), displayShorthands, definitionShorthands, typeDefinitions};
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, {title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <ChapterContext.Provider value={context}>
        <EntryContext.Provider value={entryContext}>
          <Button className="ml-3 float-right" size="sm" onClick={() => this.setState({editing: !editing})}><i className={"fas fa-" + (editing ? "times" : "edit")}/></Button>
          <h3><InlineTextEditor text={title} callback={this.updateTitle}/></h3>
          <p>{summary}</p>
          <DndProvider backend={Backend}>
            <DraggableList.Simple
              type="ChapterEntry"
              enabled={editing}
              onDrop={this.onDropEntry}
              entries={entries.map((entry, index) => {return {key: entry.url, data: {url: entry.url, index}, element: <ChapterEntry entry={entry} />}})}
            />
          </DndProvider>
          <hr/>
          <ChapterEntryAdder />
        </EntryContext.Provider>
      </ChapterContext.Provider>
    </Page>;
  }
}
