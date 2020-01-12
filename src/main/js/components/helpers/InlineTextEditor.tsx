import * as React from "react";
import {formatHtml} from "./Formatter";
import {ClickableText} from "../pages/theorem/steps/components/ClickableText";
import Form from "react-bootstrap/Form";

export interface InlineTextEditorProps {
  text: string,
  callback: (newName: string) => Promise<void>
}
interface InlineTextEditorState {
  editing: boolean
  newText: string
}

export class InlineTextEditor extends React.Component<InlineTextEditorProps, InlineTextEditorState> {
  input: any;
  constructor(props: InlineTextEditorProps) {
    super(props);
    this.state = {
      editing: false,
      newText: ''
    };
  }
  componentDidUpdate() {
    if (this.state.editing) {
      const input = this.input! as HTMLInputElement;
      const inputStyle = window.getComputedStyle(input);
      const temporarySpacingElement = document.createElement("span");
      temporarySpacingElement.style.border = "0";
      temporarySpacingElement.style.padding = "2px";
      temporarySpacingElement.style.background = "#fff";
      temporarySpacingElement.style.font = inputStyle.font;
      temporarySpacingElement.style.visibility = "hidden";
      temporarySpacingElement.style.whiteSpace = "pre";
      temporarySpacingElement.innerHTML = input.value.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
      document.body.appendChild(temporarySpacingElement);
      const contentWidth = temporarySpacingElement.getBoundingClientRect().width;
      document.body.removeChild(temporarySpacingElement);
      input.style.width = contentWidth + "px";
      if (input !== document.activeElement) {
        input.focus();
        input.select();
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
    const {text: originalName} = this.props;
    const {newText} = this.state;
    if (newText !== originalName) {
      this.props.callback(this.state.newText).then(() => this.setState({editing: false}));
    } else {
      this.setState({editing: false});
    }
  };
  render() {
    const {text} = this.props;
    const {editing, newText} = this.state;
    return editing ?
      <Form.Control type="text"
                    value={newText}
                    onChange={(e: any) => this.setState({newText: (e.target as HTMLInputElement).value})}
                    onKeyUp={this.onInputKeyUp}
                    onBlur={this.saveValue}
                    ref={(ref: any) => { this.input = ref }}
                    style={{display: "inline", padding: 0, height: "inherit", lineHeight: "inherit", fontSize: "inherit"}}/> :
      <ClickableText onClick={() => this.setState({editing: true, newText: text})}>
        {formatHtml(text)}
      </ClickableText>;
  }
}
