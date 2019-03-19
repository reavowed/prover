function formatWithReplacement(text, regex, handlePlain, handleMatch) {
  const matches = text.matchAll(regex);
  let indexOfLastMatchEnd = 0;
  let html = "";
  for (const match of matches) {
    html += handlePlain(text.substr(indexOfLastMatchEnd, match.index - indexOfLastMatchEnd));
    html += handleMatch(match);
    indexOfLastMatchEnd = match.index + match[0].length;
  }
  html += handlePlain(text.substr(indexOfLastMatchEnd));
  return html;
}

function formatHtml(text, replacementFunction) {
  if (!replacementFunction) {
    replacementFunction = x => x;
  }
  return formatWithReplacement(text, /([_^])([^\s)}]+)/g, replacementFunction, match => {
    if (match[1] === "_") {
      return `<sub>${match[2]}</sub>`
    } else if (match[1] === "^") {
      return `<sup>${match[2]}</sup>`
    }
  });
}
function replacePlaceholders(text, components) {
  return formatWithReplacement(text, /%(\d+)/g, x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

class Expression extends React.Component {
  static renderVariable(variable) {
    return formatHtml(variable.name);
  }
  static renderParameter(parameter, boundVariableLists) {
    return formatHtml(boundVariableLists[parameter.level][parameter.index]);
  }
  static renderDefinedExpression(expression, boundVariableLists, safe) {
    let formatString = (safe && expression.definition.format.requiresBrackets) ?
      "(" + expression.definition.format.baseFormatString + ")" :
      expression.definition.format.baseFormatString;
    let innerBoundVariableLists = expression.scopedBoundVariableNames.length ?
      [expression.scopedBoundVariableNames].concat(boundVariableLists) :
      boundVariableLists;
    let components = expression.scopedBoundVariableNames.concat(expression.components.map(c => this.renderExpression(c, innerBoundVariableLists, true)));
    return formatHtml(formatString, s => replacePlaceholders(s, components));
  }
  static renderApplicationExpression(expression, boundVariableLists) {
    let formatString = expression.variableName + "(" + expression.arguments.map((_, i) => "%" + i).join(", ") + ")";
    let components = expression.arguments.map(c => this.renderExpression(c, boundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, components));
  }
  static renderExpression(expression, boundVariableLists, safe) {
    if ("name" in expression) { // Statement or term variable
      return this.renderVariable(expression);
    } else if ("definition" in expression) { // Defined statement or term
      return this.renderDefinedExpression(expression, boundVariableLists, safe);
    } else if ("level" in expression) { // Function parameter
      return this.renderParameter(expression, boundVariableLists)
    } else if ("arguments" in expression) { // Application
      return this.renderApplicationExpression(expression, boundVariableLists)
    } else {
      return "?";
    }
  }

  render() {
    return <span className={this.props.className} dangerouslySetInnerHTML={{ __html: Expression.renderExpression(this.props.expression, [], false)}}></span>;
  }
}

const ProofLine = styled.div`
  display: block;
  padding-left: 5px;
  padding-bottom: 5px;
  
  ${props => props.highlighted && styled.css`
    ${ProofLineStatement} {
      color: red;
    }
  `}
`;

const ProofLineStatement = styled(Expression)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;

class Step extends React.Component {

  innerContent() {
    switch (this.props.step.type) {
      case "target":
        return <span>Then <ProofLineStatement expression={this.props.step.statement}/>.</span>;
      case "assertion":
        return <span>Then <ProofLineStatement expression={this.props.step.statement}/>.</span>;
    }
  }

  isHighlighted() {
    return _.some(this.props.highlightedPremises, p => p.lineReference = this.props.path.join("."))
  }

  render() {
    return <ProofLine highlighted={this.isHighlighted()}
                      onMouseEnter={() => this.props.setHighlightedPremises(this.props.step.referencedLines || [])}
                      onMouseLeave={() => this.props.setHighlightedPremises([])}
    >{this.innerContent()}</ProofLine>
  }
}

class Proof extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  render() {
    return this.props.steps.map((step, index) =>
      <Step step={step} path={[index]} setHighlightedPremises={this.setHighlightedPremises} highlightedPremises={this.state.highlightedPremises}/>
    );
  }
}

ReactDOM.render(<Proof steps={proof}/>, document.getElementById("proof"));
