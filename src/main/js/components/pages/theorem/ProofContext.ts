import React from "react";
import {Parser} from "../../../Parser";
import {VariableDefinitions} from "../../definitions/DefinitionParts";
import {Step} from "../../../models/Step";
import {Reference} from "../../definitions/Reference";
import {ActionHighlight} from "./TheoremContext";
import {JsonRequestInit} from "../../../utils";

export type StepActions = {
    [key: string]: () => void
}
export type ProofContextType = {
    index: number
    parser: Parser
    variableDefinitions: VariableDefinitions
    stepToHighlight?: string
    registerStep(actions: StepActions, path: number[]): void
    unregisterStep(actions: StepActions, path: number[]): void
    callOnStep(path: number[], action: string): void
    fetchJson(subpath: string, options?: JsonRequestInit): Promise<any>
    fetchJsonForStep(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<any>
    fetchJsonForStepAndInsert(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<[number[], Step[]]>
    fetchJsonForStepAndReplace(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<void>
    fetchJsonForStepAndInsertAndReplace(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<void>
    fetchJsonForStepAndInsertAndReplaceMultiple(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<void>
    fetchJsonAndInsertAndDelete(subpath: string, options?: JsonRequestInit): Promise<void>
    fetchJsonForStepAndReplaceWithWrapping(stepPath: number[], subpath: string, options?: JsonRequestInit): Promise<void>
    setHighlighting(newHighlightedPremises: Reference[], newHighlightedConclusion?: Reference): void
    getHighlighting(): [ActionHighlight[], Reference[]]
    setHighlightingAction(actionHighlights: ActionHighlight[], staticHighlights: Reference[]): void
    clearHighlightingAction(): void
}

export default React.createContext<ProofContextType | null>(null);
