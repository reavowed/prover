import React, {CSSProperties, useCallback, useContext, useRef, useState} from "react";
import {useDrag, useDrop} from "react-dnd";
import update from 'immutability-helper';
import BoundVariableListContext from "../expressions/boundVariables/BoundVariableListContext";
import {DropTargetMonitor} from "react-dnd/lib/interfaces/monitors";
import {ConnectDragSource} from "react-dnd/lib/interfaces";
import _ from "lodash";

type DragContextType = {
  type: string
  enabled?: boolean
  insideDrag?: boolean
}

type ListContextType<Data> = DragContextType & {
  parentContext: ListContextType<Data> | null
  path: number[]
  hiddenIndex: number | null
  placeholderProps: PlaceholderProps | null
  entryContext: EntryContextType<Data> | null
  onDragStart(entry: EntryContextType<Data>, boundVariableLists: string[][]): void
  onDrop(itemDropped: EntryContextType<Data>, itemToReplace: EntryContextType<Data> | null): void
  addPlaceholder(props: PlaceholderProps, index: number): void
  removePlaceholder(): PlaceholderProps | null
  reset(): void
}
const ListContext = React.createContext<ListContextType<any> | null>(null);

type EntryContextType<Data> = DragContextType & {
  entry: Entry<Data>
  path: number[],
  listContext: ListContextType<Data>
  originalListContext: ListContextType<Data>
  index: number
  onDragStart(): void
  onDrop(): void
  // Internal state props - probably best stored elsewhere?
  movingAfter?: boolean
  itemToReplace?: EntryContextType<Data> | null
  dropHandled?: boolean
  isPlaceholder?: boolean
}

const EntryContext = React.createContext<EntryContextType<any> & {drag: ConnectDragSource} | null>(null);

type PlaceholderProps = {
  boundVariableLists: string[][]
  element: React.ReactNode
  index: number
}

export type Entry<Data> = {
  key: string
  element: React.ReactNode
  data: Data
}

export type DraggableListProps<Data> = {
  type: string
  enabled: boolean
  onDrop: (itemDropped: Data, itemDroppedOn: Data | null, movingAfter?: boolean) => Promise<any>
  children: React.ReactNode
}
export default function DraggableList<Data>({type, enabled, onDrop, children}: DraggableListProps<Data>) {
  const [waitingForDrop, setWaitingForDrop] = useState(false);
  const [hiddenIndex, setHiddenIndex] = useState<number | null>(null);
  const [placeholderProps, setPlaceholderProps] = useState<PlaceholderProps | null>(null);
  function reset() {
    setHiddenIndex(null);
    setPlaceholderProps(null);
  }
  function addPlaceholder(props: PlaceholderProps, index: number) {
    setPlaceholderProps({...props, index});
  }
  const removePlaceholder = useCallback(
    () => {
      let currentPlaceholderProps: PlaceholderProps | null = null;
      setPlaceholderProps((p: PlaceholderProps | null) => {
        currentPlaceholderProps = p;
        return null;
      });
      return currentPlaceholderProps;
    },
    [placeholderProps]
  );
  function onDragStart(entryContext: EntryContextType<Data>, boundVariableLists: string[][]) {
    setHiddenIndex(entryContext.index);
    setPlaceholderProps({
      index: entryContext.index,
      element: entryContext.entry.element,
      boundVariableLists
    });
  }
  function onDropEntry(itemDropped: EntryContextType<Data>, itemToReplace: EntryContextType<Data>) {
    setWaitingForDrop(true);
    onDrop(itemDropped.entry.data, itemToReplace && itemToReplace.entry.data, itemDropped.movingAfter)
      .catch(() => {})
      .then(() => {
        reset();
        itemDropped.originalListContext.reset();
        setWaitingForDrop(false)
      });
  }

  const parentContext = useContext(ListContext);
  const entryContext = useContext(EntryContext);
  const path = entryContext ? entryContext.path : [];

  const listContext: ListContextType<Data> = {
    type,
    path,
    hiddenIndex,
    placeholderProps,
    enabled: enabled && !waitingForDrop && !(entryContext && entryContext.insideDrag),
    entryContext,
    parentContext,
    addPlaceholder,
    removePlaceholder,
    onDragStart,
    onDrop: onDropEntry,
    reset
  };

  return <ListContext.Provider value={listContext}>
    {children}
  </ListContext.Provider>;
};

type InternalEntryProps = {
  key: string,
  element: React.ReactNode
  placeholder?: boolean
  hidden?: boolean
}

DraggableList.Entries = function Entries<T>({entries}: {entries: Entry<T>[]}) {
  const listContext = useContext(ListContext)!;
  const {type, path: outerPath, hiddenIndex, placeholderProps, enabled} = listContext!;
  const boundVariableLists = useContext(BoundVariableListContext);

  function wrapAndAddPlaceholder(entryProps: InternalEntryProps[]) {
    if (_.isNumber(hiddenIndex)) {
      entryProps = update(entryProps, {$splice: [[hiddenIndex, 1, {...entryProps[hiddenIndex], hidden: true}]]});
    }
    if (placeholderProps) {
      // This hack is because we have to treat the placeholder as an entry in order to preserve hook calls,
      // but it can't actually be dragged
      const placeholderEntryContext = {
        type,
        isPlaceholder: true
      } as any as EntryContextType<T>;
      entryProps = update(entryProps, { $splice : [[
          placeholderProps.index,
          0,
          {
            element: <Entry context={placeholderEntryContext}>
              <BoundVariableListContext.Provider value={placeholderProps.boundVariableLists}>
                {placeholderProps.element}
              </BoundVariableListContext.Provider>
            </Entry>,
            key: "placeholder",
            placeholder: true
          }]]});
    }
    return <>{
      _.map(entryProps, ({element, key, hidden, placeholder}) => {
        const style: CSSProperties = {};
        if (hidden)
          style.display = "none";
        else if (placeholder)
          style.opacity = 0.2;
        return <div style={style} key={key}>{element}</div>;
      })
    }</>;
  }

  if (enabled) {
    return wrapAndAddPlaceholder(
      entries.map((entry, index) => {
        const {key, element} = entry;
        const path = [...outerPath, index];
        function onDragStart() {
          listContext.onDragStart(entryContext, boundVariableLists);
        }
        function onDrop() {
          if (!entryContext.dropHandled) {
            entryContext.dropHandled = true;
            return entryContext.listContext.onDrop(entryContext, entryContext.itemToReplace || null);
          }
        }
        const entryContext: EntryContextType<T> = {entry, type, index, path, enabled, onDragStart, onDrop, listContext, originalListContext: listContext};
        return {element: <Entry context={entryContext}>{element}</Entry>, key};
      })
    );
  } else {
    return wrapAndAddPlaceholder(entries);
  }
};

function Entry<Data>({context, children}: {context: EntryContextType<Data>, children: React.ReactNode}) {
  const {onDragStart} = context;
  const [, drag] = useDrag<EntryContextType<Data>, any, any>({
    item: context,
    begin() {
      setTimeout(() => onDragStart(), 0);
    },
    end(item) {
      if (!item.dropHandled) {
        item.listContext.reset();
        item.originalListContext.reset();
      }
    }
  });
  const [, drop] = useDrop<EntryContextType<Data>, any, any>({
    accept: context.type,
    drop(item) {
      item.onDrop();
    }
  });
  return <div ref={drop}>
    <EntryContext.Provider value={{...context, drag}}>
      {children}
    </EntryContext.Provider>
  </div>;
}

function createHoverZone<T extends DragContextType, Data>(
    contextType: React.Context<T | null>,
    children: React.ReactNode,
    onTopHover: (context: T, itemBeingDragged: EntryContextType<Data>) => void,
    onBottomHover: (context: T, itemBeingDragged: EntryContextType<Data>) => void
) {
  const context = useContext(contextType);
  if (context) {
    const dropRef = useRef<HTMLDivElement | null>(null);
    const [, drop] = useDrop<EntryContextType<Data>, any, any>({
      accept: context.type,
      hover(itemBeingDragged: EntryContextType<Data>, monitor: DropTargetMonitor) {
        const hoverBoundingRect = dropRef.current!.getBoundingClientRect();
        const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
        const clientOffset = monitor.getClientOffset()!;
        const hoverClientY = clientOffset.y - hoverBoundingRect.top;

        if (hoverClientY < hoverMiddleY) {
          onBottomHover(context, itemBeingDragged);
        } else {
          onTopHover(context, itemBeingDragged);
        }
      }
    });
    if (context.enabled && !context.insideDrag) {
      drop(dropRef);
      return <div ref={dropRef}>{children}</div>;
    }
  }
  return <div>{children}</div>;
}

function move<Data>(entryBeingDragged: EntryContextType<Data>, listContext: ListContextType<Data>, newIndex: number, itemToReplace: EntryContextType<Data> | null, movingAfter: boolean) {
  const newPath = [...listContext.path, newIndex];
  if (!_.isEqual(entryBeingDragged.path, newPath)) {
    const placeholderElement = entryBeingDragged.listContext.removePlaceholder()!;
    listContext.addPlaceholder(placeholderElement, newIndex);
    entryBeingDragged.index = newIndex;
    entryBeingDragged.path = newPath;
    entryBeingDragged.itemToReplace = itemToReplace;
    entryBeingDragged.movingAfter = movingAfter;
    entryBeingDragged.listContext = listContext;
  }
}

DraggableList.SingleDropZone = function SingleDropZone({children}: {children: React.ReactNode}) {
  return createHoverZone(
      EntryContext,
      children,
      (context, itemBeingDragged) => {
        move(itemBeingDragged, context.listContext, context.index, context, false)
      },
      (context, itemBeingDragged) => {
        move(itemBeingDragged, context.listContext, context.index + 1, context, true)
      });
};

DraggableList.Before = function Before({children}: {children: React.ReactNode}) {
  return createHoverZone(
      ListContext,
      children,
      (context, itemBeingDragged) => {
        move(itemBeingDragged, context.parentContext!, context.entryContext!.index, context.entryContext, false)
      },
      (context, itemBeingDragged) => {
        move(itemBeingDragged, context, 0, null, false)
      });
}

DraggableList.DragHandle = function DragHandle({as, children}: {as?: string, children: React.ReactElement}) {
  const elementTag = as || "div";
  const context = useContext(EntryContext);
  if (context && !context.insideDrag) {
    const {drag} = context;
    return React.createElement(elementTag, {ref: drag, style: {cursor: "move"}}, children);
  } else {
    return children;
  }
};

