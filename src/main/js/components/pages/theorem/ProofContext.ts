import React from "react";
import {Parser} from "../../../Parser";
import {VariableDefinitions} from "../../definitions/DefinitionParts";
import {Step} from "../../../models/Step";
import {Reference} from "../../definitions/Reference";
import {ActionHighlight} from "./TheoremContext";

export type ProofContextType = {
    parser: Parser
    variableDefinitions: VariableDefinitions
    registerStep(step: Step, path: number[]): void
    unregisterStep(step: Step, path: number[]): void
    callOnStep(step: Step, action: keyof Step): void
    fetchJson(subpath: string, options: RequestInit): Promise<any>
    fetchJsonForStep(stepPath: number[], subpath: string, options: RequestInit): Promise<any>
    fetchJsonForStepAndInsert(stepPath: number[], subpath: string, options: RequestInit): Promise<[number[], Step[]]>
    fetchJsonForStepAndReplace(stepPath: number[], subpath: string, options: RequestInit): Promise<void>
    fetchJsonForStepAndInsertAndReplace(stepPath: number[], subpath: string, options: RequestInit): Promise<void>
    fetchJsonForStepAndInsertAndReplaceMultiple(stepPath: number[], subpath: string, options: RequestInit): Promise<void>
    fetchJsonAndInsertAndDelete(subpath: string, options: RequestInit): Promise<void>
    fetchJsonForStepAndReplaceWithWrapping(stepPath: number[], subpath: string, options: RequestInit): Promise<void>
    updateTheorem(newTheoremJson: any): Promise<void>
    setHighlighting(newHighlightedPremises: Reference[], newHighlightedConclusion: Reference): void
    getHighlighting(): [ActionHighlight[], Reference[]]
    setHighlightingAction(actionHighlights: ActionHighlight[], staticHighlights: Reference[]): void
    clearHighlightingAction(): void
}

export default React.createContext<ProofContextType | null>(null);
