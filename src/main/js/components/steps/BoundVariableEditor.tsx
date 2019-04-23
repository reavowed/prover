import * as React from "react";
import {formatHtml} from "../helpers/Formatter";
import {ClickableText} from "./ClickableText";
import Form from "react-bootstrap/Form";

export interface BoundVariableEditorProps {
  name: string,
  callback: (newName: string) => Promise<void>
}
interface BoundVariableEditorState {
  editing: boolean
  newName: string
}

export class BoundVariableEditor extends React.Component<BoundVariableEditorProps, BoundVariableEditorState> {
  input: any;
  constructor(props: BoundVariableEditorProps) {
    super(props);
    this.state = {
      editing: false,
      newName: ''
    };
  }
  componentDidUpdate() {
    if (this.state.editing) {
      const input = this.input! as HTMLInputElement;
      const temporarySpacingElement = document.createElement("span");
      temporarySpacingElement.style.border = "0";
      temporarySpacingElement.style.padding = "2px";
      temporarySpacingElement.style.background = "#fff";
      temporarySpacingElement.style.font = "12pt sans-serif";
      temporarySpacingElement.style.visibility = "hidden";
      temporarySpacingElement.style.whiteSpace = "pre";
      temporarySpacingElement.innerHTML = input.value.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
      document.body.appendChild(temporarySpacingElement);
      const contentWidth = temporarySpacingElement.getBoundingClientRect().width;
      document.body.removeChild(temporarySpacingElement);
      input.style.width = contentWidth + "px";
      if (input !== document.activeElement) {
        input.focus();
      }
    }
  }
  onInputKeyUp = (event: any) => {
    if (event.key === "Enter") {
      this.saveValue();
    }
    event.preventDefault();
    event.stopPropagation();
  };
  saveValue = () => {
    const {name: originalName} = this.props;
    const {newName} = this.state;
    if (newName !== originalName) {
      this.props.callback(this.state.newName);
    } else {
      this.setState({editing: false});
    }
  };
  render() {
    const {name} = this.props;
    const {editing, newName} = this.state;
    return editing ?
      <Form.Control type="text"
                    value={newName}
                    onChange={(e: any) => this.setState({newName: (e.target as HTMLInputElement).value})}
                    onKeyUp={this.onInputKeyUp}
                    onBlur={this.saveValue}
                    ref={(ref: any) => { this.input = ref }}
                    style={{display: "inline", padding: 0, height: "inherit", lineHeight: "inherit"}}/> :
      <ClickableText onClick={() => this.setState({editing: true, newName: name})}>
        {formatHtml(name)}
      </ClickableText>;
  }
}
