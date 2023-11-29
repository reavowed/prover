import DraggableList from "../../../draggableList/DraggableList";
import Steps from "./Steps";
import {PropsWithChildren} from "react";

export default {
  WithoutSubsteps({children}: PropsWithChildren<{}>) {
    return <DraggableList.SingleDropZone>
      {children}
    </DraggableList.SingleDropZone>
  },
  WithSubsteps({children, path}: PropsWithChildren<{path: number[]}>) {
    return <Steps.Container path={path}>
      {children}
    </Steps.Container>
  },
  Antecedent({children}: PropsWithChildren<{}>) {
    return <DraggableList.Before>
      {children}
    </DraggableList.Before>;
  }
}
