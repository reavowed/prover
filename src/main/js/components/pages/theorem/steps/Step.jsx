import DraggableList from "../../../draggableList/DraggableList";
import {Steps} from "./Steps";

export default {
  WithoutSubsteps({children}) {
    return <DraggableList.SingleDropZone>
      {children}
    </DraggableList.SingleDropZone>
  },
  WithSubsteps({children, path}) {
    return <Steps.Container path={path}>
      {children}
    </Steps.Container>
  },
  Antecedent({children}) {
    return <DraggableList.Before>
      {children}
    </DraggableList.Before>;
  }
}
