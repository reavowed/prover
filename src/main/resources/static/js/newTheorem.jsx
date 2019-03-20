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

function serialize(expression) {
  if (typeof expression === "string") { // Variable or constant
    return expression;
  } else if (_.isArray(expression) && _.isString(expression[0])) { // Defined statement or term
    return _.map(expression, serialize).join(" ")
  } else if (_.isArray(expression) && _.isNumber(expression[0])) { // Function parameter
    return "$".repeat(expression[0]) + expression[1];
  } else if (_.isObject(expression)) { // Application
    let [[name, args]] = _.toPairs(expression);
    return `with ${name} (${_.map(args, serialize)})`
  } else {
    return "?";
  }
}

class Expression extends React.Component {
  static renderParameter(parameter, boundVariableLists) {
    let [level, index] = parameter;
    return formatHtml(boundVariableLists[level][index]);
  }
  static renderDefinedExpression(expression, boundVariableLists, safe) {
    let [symbol, ...components] = expression;
    let definition = window.definitions[symbol];
    let formatString = (safe && definition.requiresBrackets) ?
      "(" + definition.baseFormatString + ")" :
      definition.baseFormatString;
    let innerBoundVariableLists = definition.numberOfBoundVariables > 0 ?
      [components.slice(0, definition.numberOfBoundVariables), ...boundVariableLists] :
      boundVariableLists;
    let renderedComponents = components.map(c => this.renderExpression(c, innerBoundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, renderedComponents));
  }
  static renderApplicationExpression(expression, boundVariableLists) {
    var [[name, args]] = _.toPairs(expression);
    let formatString = name + "(" + args.map((_, i) => "%" + i).join(", ") + ")";
    let components = args.map(c => this.renderExpression(c, boundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, components));
  }
  static renderExpression(expression, boundVariableLists, safe) {
    if (typeof expression === "string") { // Variable or constant
      return formatHtml(expression);
    } else if (_.isArray(expression) && _.isString(expression[0])) { // Defined statement or term
      return this.renderDefinedExpression(expression, boundVariableLists, safe);
    } else if (_.isArray(expression) && _.isNumber(expression[0])) { // Function parameter
      return this.renderParameter(expression, boundVariableLists)
    } else if (_.isObject(expression)) { // Application
      return this.renderApplicationExpression(expression, boundVariableLists)
    } else {
      return "?";
    }
  }

  render() {
    return <span className={this.props.className} dangerouslySetInnerHTML={{ __html: Expression.renderExpression(this.props.expression, [], false)}} />;
  }
}

const ProofLine = styled(class extends React.Component {
  render() {
    return <div onMouseEnter={() => this.props.setHighlightedPremises(this.props.step.referencedLines || [])}
                onMouseLeave={() => this.props.setHighlightedPremises([])}
                className={this.props.className}>
      {this.props.children}
    </div>;
  }
})`
  padding-bottom: 5px;
`;

const HighlightableStatement = styled(Expression)`
  color: ${props => _.some(props.highlightedPremises, p => p.lineReference === props.reference) && "red"};
`;

const ProofLineStatement = styled(HighlightableStatement)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;

class AssumptionStep extends React.Component {
  render() {
    let {step, path, substeps, ...otherProps} = this.props;
    return <div>
      <ProofLine step={step} {...otherProps}>
        <span>Assume <ProofLineStatement expression={step.statement} reference={path.join(".") + "a"} {...otherProps}/>.</span>
      </ProofLine>
      <StepChildren steps={substeps} path={path} {...otherProps} />
    </div>;
  }
}

class AssertionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    return <ProofLine step={step} {...otherProps}>
      <span>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</span>
    </ProofLine>;
  }
}

class Steps extends React.Component {
  static getElementName(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return AssertionStep;
      case "assumption":
        return AssumptionStep;
    }
  }
  render() {
    let {steps, className, path, ...otherProps} = this.props;
    return <div className={className}>
      {steps.map((step, index) => {
        let newProps = {
          step: step,
          path: [...path, index],
          key: step.type + " " + serialize(step.statement),
          ...otherProps
        };
        return React.createElement(Steps.getElementName(step), newProps);
      })}
    </div>;
  }
}

const StepChildren = styled(Steps)`
  margin-left: 20px;
`;

class Premise extends React.Component {
  render() {
    return <HighlightableStatement reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} expression={this.props.premise}/>;
  }
}

const InferenceSummary = styled(class extends React.Component {
  renderSinglePremise(premise) {
    return <div>Suppose {premise}.</div>;
  }
  renderMultiplePremises(premises) {
    let initialPremises = _.flatMap(premises.slice(0, -1), p => [p, <span>, </span>]).slice(0, -1);
    let lastPremise =  premises.slice(-1)[0];
    return <div>Suppose {initialPremises} and {lastPremise}.</div>;
  }
  render() {
    let {inference} = this.props;
    let premiseElements = inference.premises.map((p, i) => <Premise premise={p} index={i} highlightedPremises={this.props.highlightedPremises}/>);
    let premiseElement = premiseElements.length > 0 && (premiseElements.length > 1 ? this.renderMultiplePremises(premiseElements) : this.renderSinglePremise(premiseElements[0]));
    return <div className={this.props.className}>
      {premiseElement}
      <div>{premiseElements.length > 0 && "Then "}<Expression expression={inference.conclusion}/>.</div>
    </div>
  }
})`margin-top: 5px;`;


class Theorem extends React.Component {
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
    let {theorem, previousEntry, nextEntry, usages} = this.props;
    let {proof} = theorem;
    return <div className="inference">
      <div className="navigationLinks">
        {previousEntry && <a className="navigationLink pull-left" href={previousEntry.key}>&laquo; {previousEntry.name}</a>}
        {nextEntry && <a className="navigationLink pull-right" href={nextEntry.key}>{nextEntry.name} &raquo;</a>}
      </div>
      <div className="inferenceTitle">
        <h3>
          Theorem: {theorem.name}
        </h3>
        <div className="inferenceId">
          {theorem.id}
        </div>
      </div>

      <InferenceSummary inference={theorem} highlightedPremises={this.state.highlightedPremises}/>

      <hr/>

      <h4>Proof</h4>
      <Steps steps={proof} path={[]} setHighlightedPremises={this.setHighlightedPremises} highlightedPremises={this.state.highlightedPremises} />

      {usages.length > 0 &&
        <div>
          <hr />
          {usages.map(([usageBook, usageChapter, theorems]) =>
            <div key={usageBook.key.value + "/" + usageChapter.key.value}>
              <div><label>{usageBook.title} - {usageChapter.title}</label></div>
              <p>{theorems.map(theorem => <span className="usage" key={theorem.key.value}> <a className="usageLink" href={theorem.key}>{theorem.name}</a> </span>)}</p>
            </div>
          )}
        </div>
      }
    </div>
  }
}

ReactDOM.render(<Theorem theorem={theorem} previousEntry={previousEntry} nextEntry={nextEntry} usages={usages}/>, document.getElementById("theorem"));
