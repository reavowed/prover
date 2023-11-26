import _ from "lodash";

export class PremiseReference {
    type = "premise";
    constructor(public index: number, public innerPath: number[] | null = null) {}
    matches(other: Reference): boolean {
        return other instanceof PremiseReference && other.index == this.index;
    }
    toString(): string {
        return "p" + this.index + (this.innerPath ? "-" + this.innerPath.join(".") : "");
    }
}

export class StepReference {
    type = "step";
    constructor(public stepPath: number[], public suffix: string | null = null, public innerPath: number[] | null = null) {}
    matches(other: Reference): boolean {
        return other instanceof StepReference && _.isEqual(other.stepPath, this.stepPath) && other.suffix == this.suffix;
    }
    toString(): string {
        return this.stepPath.join(".") + (this.suffix || "") + (this.innerPath ? "-" + this.innerPath.join(".") : "");
    }
}

export type Reference = PremiseReference | StepReference;
