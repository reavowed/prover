import React from "react";

export default class SuggestionDropdownElement extends React.Component {
  constructor(props) {
    super(props);
    this.state = {isHovered: false};
  }
  render() {
    const {mainElement, hoverElement} = this.props;
    return <div onMouseEnter={() => this.setState({isHovered: true})} onMouseLeave={() => this.setState({isHovered: false})}>
      {mainElement} {this.state.isHovered && <> ({hoverElement})</>}
    </div>
  }
}
