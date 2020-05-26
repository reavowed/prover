import * as React from "react";
import {formatHtml} from "./Formatter";
import {ClickableText} from "../pages/theorem/steps/components/ClickableText";
import Form from "react-bootstrap/Form";

export interface InlineTextEditorProps {
  text: string,
  callback: (newName: string) => Promise<void>
}
interface InlineTextEditorState {
  saving: boolean
  editing: boolean
  newText: string
}

// Taken from https://stackoverflow.com/a/58533415
function getFontFromComputedStyle(computedStyle: CSSStyleDeclaration): string {
  let font = computedStyle.font;
  // Firefox returns the empty string for .font, so create the .font property manually
  if (font === '') {
    // Firefox uses percentages for font-stretch, but Canvas does not accept percentages
    // so convert to keywords, as listed at:
    //   https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch
    let fontStretchLookupTable: { [key: string]: string } = {
      '50%': 'ultra-condensed',
      '62.5%': 'extra-condensed',
      '75%': 'condensed',
      '87.5%': 'semi-condensed',
      '100%': 'normal',
      '112.5%': 'semi-expanded',
      '125%': 'expanded',
      '150%': 'extra-expanded',
      '200%': 'ultra-expanded'
    };
    // If the retrieved font-stretch percentage isn't found in the lookup table, use
    // 'normal' as a last resort.
    let fontStretch = fontStretchLookupTable.hasOwnProperty(computedStyle.fontStretch)
        ? fontStretchLookupTable[computedStyle.fontStretch]
        : 'normal';
    font = computedStyle.fontStyle
        + ' ' + computedStyle.fontVariant
        + ' ' + computedStyle.fontWeight
        + ' ' + fontStretch
        + ' ' + computedStyle.fontSize
        + '/' + computedStyle.lineHeight
        + ' ' + computedStyle.fontFamily;
  }
  return font;
}

export class InlineTextEditor extends React.Component<InlineTextEditorProps, InlineTextEditorState> {
  input: any;
  constructor(props: InlineTextEditorProps) {
    super(props);
    this.state = {
      saving: false,
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
      temporarySpacingElement.style.font = getFontFromComputedStyle(inputStyle);
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
  setStatePromise<K extends keyof InlineTextEditorState>(newState: Pick<InlineTextEditorState, K>) {
    return new Promise(resolve => this.setState(newState, resolve));
  };
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
    if (newText !== originalName && !this.state.saving) {
      this.setStatePromise({saving: true})
          .then(() => this.props.callback(this.state.newText))
          .then(() => this.setStatePromise({editing: false, saving: false}));
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
      <ClickableText onFocus={() => this.setState({editing: true, newText: text})} tabIndex={0}>
        {formatHtml(text)}
      </ClickableText>;
  }
}
