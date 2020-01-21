import React, {useCallback, useContext, useRef, useState} from "react";
import {useDrag, useDrop} from "react-dnd";
import update from 'immutability-helper';
import BoundVariableLists from "./pages/theorem/steps/BoundVariableLists";

const ListContext = React.createContext();
const EntryContext = React.createContext();

export default function DraggableList({type, enabled, onDrop, children}) {
  const [waitingForDrop, setWaitingForDrop] = useState(false);
  const [hiddenIndex, setHiddenIndex] = useState(null);
  const [placeholderProps, setPlaceholderProps] = useState(null);
  function reset() {
    setHiddenIndex(null);
    setPlaceholderProps(null);
  }
  function addPlaceholder(props, index) {
    setPlaceholderProps({...props, index});
  }
  const removePlaceholder = useCallback(
    () => {
      let currentPlaceholderProps;
      setPlaceholderProps(p => {
        currentPlaceholderProps = p;
        return null;
      });
      return currentPlaceholderProps;
    },
    [placeholderProps]
  );
  function onDragStart(entry, index, boundVariableLists) {
    setHiddenIndex(index);
    setPlaceholderProps({
      index: index,
      element: entry.element,
      boundVariableLists
    });
  }
  function onDropEntry(itemDropped, itemToReplace) {
    setWaitingForDrop(true);
    onDrop(itemDropped.data, itemToReplace.data, itemDropped.movingAfter)
      .catch(() => {})
      .then(() => {
        reset();
        itemDropped.originalListCallbacks.reset();
        setWaitingForDrop(false)
      });
  }
  const listCallbacks = {addPlaceholder, removePlaceholder, onDragStart, onDrop: onDropEntry, reset};

  const parentContext = useContext(EntryContext);
  const path = parentContext ? parentContext.path : [];

  const context = {
    type,
    path,
    listCallbacks,
    hiddenIndex,
    placeholderProps,
    enabled: enabled && !waitingForDrop && !(parentContext && parentContext.insideDrag),
    parentContext
  };

  return <ListContext.Provider value={context}>
    {children}
  </ListContext.Provider>;
};

DraggableList.Entries = function Entries({entries}) {
  const {type, path: outerPath, listCallbacks, hiddenIndex, placeholderProps, enabled} = useContext(ListContext);
  const boundVariableLists = useContext(BoundVariableLists);

  function wrapAndAddPlaceholder(entryProps) {
    if (_.isNumber(hiddenIndex)) {
      entryProps = update(entryProps, {$splice: [[hiddenIndex, 1, {...entryProps[hiddenIndex], hidden: true}]]});
    }
    if (placeholderProps) {
      entryProps = update(entryProps, { $splice : [[
          placeholderProps.index,
          0,
          {
            element: <Entry context={{insideDrag: true, type}}>
              <BoundVariableLists.Provider value={placeholderProps.boundVariableLists}>
                {placeholderProps.element}
              </BoundVariableLists.Provider>
            </Entry>,
            key: "placeholder",
            placeholder: true
          }]]});
    }
    return _.map(entryProps, ({element, key, hidden, placeholder}) => {
      const style = {};
      if (hidden)
        style.display = "none";
      else if (placeholder)
        style.opacity = 0.2;
      return <div style={style} key={key}>{element}</div>;
    });
  }

  function onDragStart(index) {
    listCallbacks.onDragStart(entries[index], index, boundVariableLists);
  }

  if (enabled) {
    return wrapAndAddPlaceholder(
      entries.map((entry, index) => {
        const {key, element, data} = entry;
        const path = [...outerPath, index];
        const context = {type, index, path, data, enabled, onDragStart, listCallbacks, originalListCallbacks: listCallbacks};
        return {element: <Entry context={context}>{element}</Entry>, key};
      })
    );
  } else {
    return wrapAndAddPlaceholder(entries);
  }
};

function Entry({context, children}) {
  const {index, onDragStart} = context;
  const [, drag] = useDrag({
    item: context,
    begin() {
      setTimeout(() => onDragStart(index), 0);
    },
    end(item) {
      if (!item.dropHandled) {
        item.listCallbacks.reset();
        item.originalListCallbacks.reset();
      }
    }
  });
  const [, drop] = useDrop({
    accept: context.type,
    drop(item) {
      if (!item.dropHandled) {
        item.dropHandled = true;
        if (item.itemToReplace) {
          return item.itemToReplace.listCallbacks.onDrop(item, item.itemToReplace);
        } else {
          item.listCallbacks.reset();
        }
      }
    }
  });
  return <div ref={drop}>
    <EntryContext.Provider value={{...context, drag}}>
      {children}
    </EntryContext.Provider>
  </div>;
}

DraggableList.Simple = function({entries, ...listProps}) {
  const wrappedEntries = entries.map(e => Object.assign({}, e, {
    element: <DraggableList.DragHandle>
      <DraggableList.SingleDropZone>
        {e.element}
      </DraggableList.SingleDropZone>
    </DraggableList.DragHandle>
  }));
  return <DraggableList {...listProps}>
    <DraggableList.Entries entries={wrappedEntries}/>
  </DraggableList>
};

function createHoverZone(contextType, children, hover) {
  const context = useContext(contextType);
  if (context) {
    const dropRef = useRef(null);
    const [, drop] = useDrop({
      accept: context.type,
      hover(itemBeingDragged, monitor) {
        hover(context, dropRef, itemBeingDragged, monitor)
      }
    });
    if (context.enabled && !context.insideDrag) {
      drop(dropRef);
      return <div ref={dropRef}>{children}</div>;
    }
  }
  return <div>{children}</div>;
}

function move(itemBeingDragged, context, newIndex, movingAfter) {
  const placeholderElement = itemBeingDragged.listCallbacks.removePlaceholder();
  context.listCallbacks.addPlaceholder(placeholderElement, newIndex);

  itemBeingDragged.index = newIndex;
  itemBeingDragged.path = [...context.path.slice(0, context.path.length - 1), newIndex];
  itemBeingDragged.itemToReplace = context;
  itemBeingDragged.movingAfter = movingAfter;
  itemBeingDragged.listCallbacks = context.listCallbacks;
}

DraggableList.SingleDropZone = function SingleDropZone({children}) {
  return createHoverZone(EntryContext, children, (context, dropRef, itemBeingDragged, monitor) => {
    const hoverBoundingRect = dropRef.current.getBoundingClientRect();
    const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
    const clientOffset = monitor.getClientOffset();
    const hoverClientY = clientOffset.y - hoverBoundingRect.top;

    if (hoverClientY < hoverMiddleY) {
      move(itemBeingDragged, context, context.index, false);
    } else {
      move(itemBeingDragged, context, context.index + 1, true);
    }
  });
};

DraggableList.Before = function Before({children}) {
  return createHoverZone(ListContext, children, (context, dropRef, itemBeingDragged, monitor) => {
    const hoverBoundingRect = dropRef.current.getBoundingClientRect();
    const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
    const clientOffset = monitor.getClientOffset();
    const hoverClientY = clientOffset.y - hoverBoundingRect.top;
    if (hoverClientY < hoverMiddleY) {
      move(itemBeingDragged, context.parentContext, context.parentContext.index, false);
    } else {
      move(itemBeingDragged, context, 0, false);
    }
  });
};

DraggableList.DragHandle = function DragHandle({as, children}) {
  const elementTag = as || "div";
  const context = useContext(EntryContext);
  if (context && !context.insideDrag) {
    const {drag} = context;
    return React.createElement(elementTag, {ref: drag, style: {cursor: "move"}}, children);
  } else {
    return children;
  }
};
