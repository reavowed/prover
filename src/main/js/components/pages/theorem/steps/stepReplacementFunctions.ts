import {Step} from "../../../../models/Step";
import {mapAtIndex} from "../../../../models/Helpers";

export function updateStep(path: number[], steps: Step[], newStep: Step): Step[] {
  return mapAtIndex(steps, path[0], step => step.updateStep(path.slice(1), newStep))
}
export function replaceStep(path: number[], steps: Step[], newSteps: Step[]): Step[] {
  const indexToReplace = path[0];
  const stepToReplace = steps[indexToReplace];
  const parentPath = stepToReplace.path.slice(0, stepToReplace.path.length - 1);
  const replacedSteps = stepToReplace.replaceStep(path.slice(1), newSteps);
  return [...steps.slice(0, indexToReplace), ...replacedSteps, ...steps.slice(indexToReplace + 1).map((s, i) => s.setPath([...parentPath, indexToReplace + replacedSteps.length + i]))];
}
export function insertSteps(path: number[], steps: Step[], newSteps: Step[]): Step[] {
  const indexToInsert = path[0];
  const parentPath = steps[0].path.slice(0, steps[0].path.length - 1);
  if (path.length === 1) {
    return [...steps.slice(0, indexToInsert), ...newSteps, ...steps.slice(indexToInsert).map((s, i) => s.setPath([...parentPath, indexToInsert + newSteps.length + i]))]
  } else {
    return mapAtIndex(steps, path[0], step => step.insertSteps(path.slice(1), newSteps));
  }
}
