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
import AvailableEntries from "../AvailableEntries";

export class Chapter extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      ...this.getStateFromProps(props),
      editing: false
    };
  }

  getStateFromProps = (props) => {
    return {
      title: props.title,
      url: props.url,
      definitions: props.definitions,
      typeDefinitions: props.typeDefinitions,
      typeRelationDefinitions: props.typeRelationDefinitions,
      standalonePropertyDefinitions: props.standalonePropertyDefinitions,
      entries: new Parser(props.definitions, props.typeDefinitions, props.typeRelationDefinitions, props.standalonePropertyDefinitions).parseEntries(props.entries),
    }
  };

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

  updateChapter = (fetchUrl, fetchData) => {
    return window.fetchJson(fetchUrl, fetchData)
      .then(newProps => {
        this.setState(this.getStateFromProps(newProps));
        if (window.location.pathname !== newProps.url) {
          history.replaceState({}, "", newProps.url);
        }
      });
  };

  onDropEntry = ({url, index}, target, after) => {
    if (target) {
      const newIndex = target.index + (after ? 1 : 0) - (index < target.index ? 1 : 0);
      if (newIndex !== index) {
        return this.updateChapter(path.join(url, "index"), {method: "PUT", body: newIndex});
      }
    }
    return Promise.resolve();
  };

  updateTitle = (newTitle) => {
    return this.updateChapter(this.state.url + "/title", {method: "PUT", body: newTitle});
  };

  render() {
    const {bookLink, summary, previous, next, displayShorthands, definitionShorthands} = this.props;
    const {title, url, entries, editing, definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions} = this.state;

    const context = {updateChapter: this.updateChapter, url, editing};
    const parser = new Parser(definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions);
    const availableEntries = AvailableEntries.create(parser, definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands);
    return <Page breadcrumbs={<Breadcrumbs links={[bookLink, {title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <ChapterContext.Provider value={context}>
        <AvailableEntries.Provider value={availableEntries}>
          <Button className="ml-3 float-right" size="sm" onClick={() => this.setState({editing: !editing})}><i className={"fas fa-" + (editing ? "times" : "edit")}/></Button>
          <h3><InlineTextEditor text={title} callback={this.updateTitle}/></h3>
          <p>{summary}</p>
          <DndProvider backend={Backend}>
            <DraggableList.Simple
              type="ChapterEntry"
              enabled={editing}
              onDrop={this.onDropEntry}
              entries={entries.map(({entry, url, type, title}, index) => {return {key: url, data: {url, index}, element: <ChapterEntry entry={entry} url={url} type={type} title={title} />}})}
            />
          </DndProvider>
          <hr/>
          <ChapterEntryAdder />
        </AvailableEntries.Provider>
      </ChapterContext.Provider>
    </Page>;
  }
}
