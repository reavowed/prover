import React, {useContext, useRef, useState} from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import Popover from "react-bootstrap/Popover";
import styled, {css} from "styled-components";
import {Reference, StepReference} from "../../../../definitions/Reference";
import DraggableList from "../../../../draggableList/DraggableList";
import {HighlightableExpression} from "../../../../expressions/ExpressionComponent";
import FlexRow from "../../../../FlexRow";
import {InlineTextEditor} from "../../../../helpers/InlineTextEditor";
import {SimpleModal} from "../../../../Modals";
import ProofContext from "../../ProofContext";
import _ from "lodash";
import {Expression} from "../../../../../models/Expression";

type ProofLineProps = {
  path: number[]
  suffix?: string
  premiseReferences?: Reference[]
  children: React.ReactNode | ((isHovered: boolean) => React.ReactNode)
  className?: string
  buttons?: React.ReactNode
  onKeyDown?: React.KeyboardEventHandler<HTMLDivElement>
  onClick?: React.MouseEventHandler<HTMLSpanElement>
  incomplete: boolean
}
const ProofLine = styled(React.forwardRef(function NewProofLine(props: ProofLineProps, containerRef: React.Ref<HTMLDivElement>) {
  const {className, children, path, suffix, buttons, premiseReferences} = props;
  const context = useContext(ProofContext)!;

  const [shouldShowSubproofNameModal, setShouldShowSubproofNameModal] = useState(false);
  const [isHovered, setIsHovered] = useState(false);
  const [isFocused, setIsFocused] = useState(false);
  const [subproofName, setSubproofName] = useState('');

  const spanRef = useRef<HTMLSpanElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);

  const clearStep = () => {
    context.fetchJsonForStepAndReplace(path, "clear", {method: "POST"})
      .then(() => context.callOnStep(path, "startProving"));
  };
  const deleteStep = () => {
    context.fetchJsonForStepAndReplace(path, "", {method: "DELETE"});
  };
  const elide = () => {
    context.fetchJsonForStepAndReplace(path, "elide", {method: "POST"});
  };
  const createSubproof = () => {
    context.fetchJsonForStepAndReplace(path, "introduceSubproof", {
      method: "POST",
      body: subproofName
    }).then(() => setShouldShowSubproofNameModal(false));
  };
  const subProofNamingModal = <SimpleModal show={shouldShowSubproofNameModal}
                                           onHide={() => setShouldShowSubproofNameModal(false)}
                                           title="Choose sub-proof name"
                                           value={subproofName}
                                           onChange={setSubproofName}
                                           onSave={createSubproof}/>;

  const onMouseEnter = () => {
    context.setHighlighting(
      premiseReferences || [],
      path && new StepReference(path, suffix || null)
    );
    setIsHovered(true);
  };
  const onMouseLeave = () => {
    context.setHighlighting([], undefined);
    setIsHovered(false);
  };
  const onKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) {
      return;
    }
    if (event.shiftKey || event.ctrlKey || event.altKey) {
      return;
    }
    if (event.key === "d") {
      deleteStep();
    } else if (event.key === "r") {
      clearStep();
    } else if (event.key === "e") {
      elide();
    } else if (props.onKeyDown) {
      props.onKeyDown(event);
    }
  };
  const onClick = (event: React.MouseEvent<HTMLSpanElement>) => {
    if (props.onClick) {
      props.onClick(event);
    }
  }

  return <div onMouseEnter={onMouseEnter}
              onMouseOver={onMouseEnter}
              onMouseLeave={onMouseLeave}
              onFocus={() => setIsFocused(true)}
              onBlur={() => setIsFocused(false)}
              onKeyDown={onKeyDown}
              tabIndex={0}
              ref={containerRef}
              className={"mb-1 " + className}>
    <FlexRow>
      <span ref={spanRef}
            onClick={onClick}
            style={props.onClick && {cursor: "pointer"}}>
        {_.isFunction(children) ? children(isHovered) : children}
      </span>
      <span className="ml-3">
        {buttons}
      </span>
      <FlexRow.Grow/>
      <span className="mb-n2" ref={buttonRef}>
        {isHovered && <DraggableList.DragHandle as="span" key="handle">
          <Button as="span" size="sm" className="ml-1"><span className="fas fa-arrows-alt-v"/></Button>
        </DraggableList.DragHandle>}
        {path && (isHovered || isFocused) && <>
          <Overlay target={buttonRef} show={isFocused} placement="bottom">
            {({show, ...props}) => <Popover id={path.join(".") + suffix} className="p-1" {...props}>
              <Button onClick={() => setShouldShowSubproofNameModal(true)} variant="success" size="sm" className="ml-1">To subproof</Button>
              <Button onClick={elide} variant="success" size="sm" className="ml-1">Elide</Button>
              <Button onClick={clearStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-redo"/></Button>
              <Button onClick={deleteStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-trash"/></Button>
            </Popover>}
          </Overlay>
        </>}
      </span>
    </FlexRow>
    {subProofNamingModal}
  </div>;
}))`
  position: relative;
  ${props => props.incomplete && css`
    &::before {
      content: "?";
      color: red;
      font-weight: bold;
      position: absolute;
      left: -10px;
    }
  `}
`;

type SingleStatementWithPrefixContentProps = {
  prefix: string
  statement: Expression
  path: number[]
  suffix?: string
  editableBoundVariable?: boolean
  additionalReferences?: Reference[]
}
const SingleStatementWithPrefixContent = function({editableBoundVariable, prefix, statement, path, suffix, additionalReferences}: SingleStatementWithPrefixContentProps) {
  const context = useContext(ProofContext)!;
  const wrapEditableBoundVariable = (name: string, index: number, boundVariablePath: number[]) => {
    const callback = (newName: string) => {
      return context.fetchJsonForStepAndReplace(path, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
        method: "PUT",
        body: newName
      });
    };
    return <InlineTextEditor text={name} callback={callback} />;
  };
  return <>
    {prefix}
    {' '}
    {statement ?
      <HighlightableExpression expression={statement}
                              references={[new StepReference(path, suffix)]}
                              additionalReferences={additionalReferences || []}
                              wrapBoundVariable={editableBoundVariable ? wrapEditableBoundVariable : undefined}/> :
      "???"}
    {'.'}
  </>
};

type SingleStatementWithPrefixProps = ProofLineProps & SingleStatementWithPrefixContentProps
function SingleStatementWithPrefix(props: SingleStatementWithPrefixProps) {
  return <ProofLine {...props}>
    <SingleStatementWithPrefixContent {...props}/>
  </ProofLine>
}

export default Object.assign(ProofLine, {
  SingleStatementWithPrefixContent,
  SingleStatementWithPrefix
});
