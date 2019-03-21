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
  render() {
    return <span className={this.props.className} dangerouslySetInnerHTML={{ __html: this.props.expression.toHtml([], false)}} />;
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
          key: step.type + " " + step.statement.serialize(),
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
    const {theorem: rawTheorem, previousEntry, nextEntry, usages} = this.props;
    const theorem = Parser.parseTheorem(rawTheorem);
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

function matchShorthand(template, expression, boundVariableLists) {
  if (_.isString(template)) {
    return expression.toHtml(boundVariableLists, false);
  } else if (_.isArray(template) && _.isString(template[0])) {
    if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
      let innerBoundVariableLists = expression.boundVariableNames.length > 0 ? [expression.boundVariableNames, ...boundVariableLists] : boundVariableLists;
      const componentMatches = _.zipWith(
        template.slice(1 + expression.definition.numberOfBoundVariables),
        expression.components,
        (t, c) => matchShorthand(t, c, innerBoundVariableLists));
      if (_.every(componentMatches)) {
        return [...expression.boundVariableNames, ..._.flatten(componentMatches)];
      }
    }
  } else if (_.isArray(template) && _.isNumber(template[0])) {
    if ((expression instanceof FunctionParameter) && _.isEqual(template, [expression.level, expression.index])) {
      return [];
    }
  }
}

class VariableOrConstant {
  constructor(name) {
    this.name = name;
  }
  serialize() {
    return this.name;
  }
  toHtml() {
    return formatHtml(this.name);
  }
}
class DefinedExpression {
  constructor(definition, boundVariableNames, components) {
    this.definition = definition;
    this.boundVariableNames = boundVariableNames;
    this.components = components;
  }
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  toHtml(boundVariableLists, safe) {
    for (let shorthand of window.shorthands) {
      const matches = matchShorthand(shorthand.template, this, boundVariableLists);
      if (matches) {
        let formatString = (safe && shorthand.requiresBrackets) ?
          "(" + shorthand.baseFormatString + ")" :
          shorthand.baseFormatString;
        return formatHtml(formatString, s => replacePlaceholders(s, matches));
      }
    }

    let formatString = (safe && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
    let innerBoundVariableLists = this.boundVariableNames.length > 0 ? [this.boundVariableNames, ...boundVariableLists] : boundVariableLists;
    let componentsHtml = this.components.map(c => c.toHtml(innerBoundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, [...this.boundVariableNames, ...componentsHtml]));
  }
}
class FunctionParameter {
  constructor(level, index) {
    this.level = level;
    this.index = index;
  }
  serialize() {
    return "$".repeat(this.level) + this.index;
  }
  toHtml(boundVariableLists) {
    return boundVariableLists[this.level][this.index];
  }
}
class ExpressionApplication {
  constructor(name, args) {
    this.name = name;
    this.args = args;
  }
  serialize() {
    return `with ${this.name} (${_.map(this.args, a => a.serialize())})`
  }
  toHtml(boundVariableLists) {
    const formatString = this.name + "(" + this.args.map((_, i) => "%" + i).join(", ") + ")";
    let argsHtml = this.args.map(c => c.toHtml(boundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, argsHtml));
  }
}

class Parser {
  static parseExpression(rawExpression) {
    if (typeof rawExpression === "string") { // Variable or constant
      return new VariableOrConstant(rawExpression);
    } else if (_.isArray(rawExpression) && _.isString(rawExpression[0])) { // Defined statement or term
      const [definitionSymbol, ...boundVariablesAndComponents] = rawExpression;
      const definition = window.definitions[definitionSymbol];
      const boundVariableNames = boundVariablesAndComponents.slice(0, definition.numberOfBoundVariables);
      const components = boundVariablesAndComponents.slice(definition.numberOfBoundVariables);
      return new DefinedExpression(definition, boundVariableNames, components.map(Parser.parseExpression));
    } else if (_.isArray(rawExpression) && _.isNumber(rawExpression[0])) { // Function parameter
      const [level, index] = rawExpression;
      return new FunctionParameter(level, index);
    } else if (_.isObject(rawExpression)) { // Application
      let [[name, args]] = _.toPairs(rawExpression);
      return new ExpressionApplication(name, args.map(Parser.parseExpression));
    } else {
      throw `Unrecognised expression ${JSON.stringify(rawExpression)}`
    }
  }
  static parseStep(step) {
    step.assumption = step.assumption && Parser.parseExpression(step.assumption);
    step.statement = step.statement && Parser.parseExpression(step.statement);
    step.substeps = step.substeps && step.substeps.map(Parser.parseStep)
  }
  static parseTheorem(rawTheorem) {
    const theorem = _.cloneDeep(rawTheorem);
    theorem.premises = theorem.premises && theorem.premises.map(Parser.parseExpression);
    theorem.conclusion = theorem.conclusion && Parser.parseExpression(theorem.conclusion);
    _.each(theorem.proof, Parser.parseStep);
    return theorem;
  }
}

ReactDOM.render(<Theorem theorem={theorem} previousEntry={previousEntry} nextEntry={nextEntry} usages={usages}/>, document.getElementById("theorem"));
