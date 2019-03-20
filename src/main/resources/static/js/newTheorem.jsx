import "@babel/polyfill";
import React from 'react';
import ReactDOM from 'react-dom';
import ReactDOMServer from 'react-dom/server';
import styled from 'styled-components';
import _ from 'lodash';

function formatWithReplacement(text, regex, handlePlain, handleMatch) {
  let match;
  let indexOfLastMatchEnd = 0;
  let html = "";
  while (match = regex.exec(text)) {
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
  if ("name" in expression) { // Statement or term variable
    return expression.name;
  } else if ("definition" in expression) { // Defined statement or term
    return [expression.definition.symbol, ...expression.components.map(serialize)].join(" ");
  } else if ("level" in expression) { // Function parameter
    return "$".repeat(expression.level) + expression.index;
  } else if ("arguments" in expression) { // Application
    return "with " + expression.variableName + "(" + expression.arguments.map(serialize).join(", ") + ")";
  } else {
    return "?";
  }
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
    return <span className={this.props.className} dangerouslySetInnerHTML={{ __html: Expression.renderExpression(this.props.expression, [], false)}} />;
  }
}

const ProofLine = styled.div`
  display: block;
  padding-bottom: 5px;
`;

const HighlightableStatement = styled(Expression)`
  color: ${props => props.highlighted && "red"};
`;

const ProofLineStatement = styled(HighlightableStatement)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;

class Step extends React.Component {
  innerContent() {
    const {step, path, ...otherProps} = this.props;
    switch (step.type) {
      case "target":
      case "assertion":
        return <span>Then <ProofLineStatement highlighted={this.isHighlighted()} expression={step.statement}/>.</span>;
      case "oldAssertion":
        return <span>Then <ProofLineStatement highlighted={this.isHighlighted()} expression={step.assertion}/>.</span>;
      case "assumption":
        return [
          <span key="assumption">Assume <ProofLineStatement highlighted={this.isAssumptionHighlighted()} expression={this.props.step.assumption}/>.</span>,
          <StepChildren key="children" steps={step.substeps} path={path} {...otherProps} />
        ]
    }
  }

  isHighlighted() {
    return _.some(this.props.highlightedPremises, p => p.lineReference === this.props.path.join("."))
  }
  isAssumptionHighlighted() {
    return _.some(this.props.highlightedPremises, p => p.lineReference === this.props.path.join(".") + "a")
  }

  render() {
    const {step, path, ...otherProps} = this.props;
    let innerContent = () => {
      switch (step.type) {
        case "target":
        case "assertion":
          return <span>Then <ProofLineStatement highlighted={this.isHighlighted()} expression={step.statement}/>.</span>;
        case "oldAssertion":
          return <span>Then <ProofLineStatement highlighted={this.isHighlighted()} expression={step.assertion}/>.</span>;
        case "assumption":
          return <span key="assumption">Assume <ProofLineStatement highlighted={this.isAssumptionHighlighted()} expression={step.assumption}/>.</span>;
      }
    };

    return <div>
      <ProofLine onMouseEnter={() => this.props.setHighlightedPremises(this.props.step.referencedLines || [])}
                      onMouseLeave={() => this.props.setHighlightedPremises([])}
      >{innerContent()}</ProofLine>
      {step.substeps && <StepChildren key="children" steps={step.substeps} path={path} {...otherProps} />}
    </div>
  }
}

class Steps extends React.Component {
  render() {
    let {steps, className, path, ...otherProps} = this.props;
    return <div className={className}>
      {steps.map((step, index) => <Step step={step} path={[...path, index]} {...otherProps} />)}
    </div>;
  }
}

const StepChildren = styled(Steps)`
  margin-left: 20px;
`;

class Premise extends React.Component {
  isHighlighted() {
    return _.some(this.props.highlightedPremises, p => p.lineReference === ("p" + this.props.index));
  }

  render() {
    return <HighlightableStatement highlighted={this.isHighlighted()} expression={this.props.premise.statement}/>;
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
        {previousEntry && <a className="navigationLink pull-left" href={previousEntry.key.url}>&laquo; {previousEntry.name}</a>}
        {nextEntry && <a className="navigationLink pull-right" href={nextEntry.key.url}>{nextEntry.name} &raquo;</a>}
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
              <p>{theorems.map(theorem => <span className="usage" key={theorem.key.value}> <a className="usageLink" href={theorem.key.url}>{theorem.name}</a> </span>)}</p>
            </div>
          )}
        </div>
      }
    </div>
  }
}

window.renderTheoremClient = function(theorem, previousEntry, nextEntry, usages) {
  ReactDOM.render(<Theorem theorem={theorem} previousEntry={previousEntry} nextEntry={nextEntry} usages={usages}/>, document.getElementById("theorem"));
};
window.renderTheoremServer = function(theorem, previousEntry, nextEntry, usages) {
  return ReactDOMServer.renderToString(<Theorem theorem={JSON.parse(theorem)} previousEntry={JSON.parse(previousEntry)} nextEntry={JSON.parse(nextEntry)} usages={JSON.parse(usages)}/>);
};
