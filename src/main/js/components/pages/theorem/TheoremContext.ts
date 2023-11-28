import * as React from "react";
import {AvailableEntries} from "../../AvailableEntriesContext";
import {Parser} from "../../../Parser";
import {VariableDefinitions} from "../../definitions/DefinitionParts";
import DisplaySettings from "../../DisplaySettings";
import {Inference} from "../../definitions/EntryDefinitions";
import {Step} from "../../../models/Step";
import {Reference} from "../../definitions/Reference";

type StepChangeProps = {
    path: number[]
    newSteps: any[]
}
type StepDeletionProps = {
    parentPath: number[]
    startIndex: number
    endIndex: number
}
type InsertionAndReplacementProps = {
    insertion: StepChangeProps
    replacement: StepChangeProps
}
type InsertionAndDeletionProps = {
    insertion: StepChangeProps
    deletion: StepDeletionProps
}
type StepWithReferenceChange = {
    step: any
    path: number[]
}
type ProofUpdateProps<T> = {
    stepUpdates: T
    newInferences: {[key: string]: Inference}
    stepsWithReferenceChanges: StepWithReferenceChange[]
}

// TODO: Move to highlightable expression file
export type ActionHighlight = {
    reference: Reference
    action?: () => void
}

export type TheoremContextType = {
    availableEntries: AvailableEntries
    parser: Parser
    variableDefinitions: VariableDefinitions
    displaySettings: DisplaySettings
    inferencesToHighlight?: string[]
    fetchJson(subpath: string, options: RequestInit): Promise<any>
    updateTheorem(newTheoremJson: any): Promise<void>
    insertSteps(proofIndex: number, props: ProofUpdateProps<StepChangeProps>): Promise<[number[], Step[]]>
    replaceSteps(proofIndex: number, props: ProofUpdateProps<StepChangeProps>): Promise<[number[], Step[]]>
    insertAndReplaceSteps(proofIndex: number, props: ProofUpdateProps<InsertionAndReplacementProps>): Promise<[number[], Step[], number[], Step[]]>
    insertAndReplaceMultipleSteps(proofIndex: number, props: ProofUpdateProps<InsertionAndReplacementProps>): Promise<[number[], Step[], number[], Step[]]>
    insertAndDeleteSteps(proofIndex: number, props: ProofUpdateProps<InsertionAndDeletionProps>): Promise<void>
    setHighlighting(newHighlightedPremises: Reference[], newHighlightedConclusion: Reference, proofIndex: number): void
    getHighlighting(proofIndex?: number): [ActionHighlight[], Reference[]]
    setHighlightingAction(actionHighlights: ActionHighlight[], staticHighlights: Reference[], proofIndex: number): void
    clearHighlightingAction(): void
}

export default React.createContext<TheoremContextType | null>(null);
