import DraggableList, {DraggableListProps, Entry} from "./DraggableList";
import React from "react";

type SimpleDraggableListProps<Data> = Omit<DraggableListProps<Data>, "children"> & { entries: Entry<Data>[] }

export function SimpleDraggableList<Data>({entries, ...listProps}: SimpleDraggableListProps<Data>) {
  const wrappedEntries = entries.map(e => Object.assign({}, e, {
    element: <DraggableList.DragHandle>
      <DraggableList.SingleDropZone>
        {e.element}
      </DraggableList.SingleDropZone>
    </DraggableList.DragHandle>
  }));
  return <DraggableList {...listProps}>
    <DraggableList.Entries entries={wrappedEntries} />
  </DraggableList>
};
