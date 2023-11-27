import React from "react";

export type SimpleReactNode = React.ReactElement | string
export type CompoundReactNode = SimpleReactNode | SimpleReactNode[]

export function isDefined<T>(t: T | undefined | null): t is T {
    return !!t
}
export function startsWith<T>(array: T[], target: T[]): boolean {
    if (array.length < target.length) return false;
    for (let i = 0; i < target.length; ++i) {
        if (array[i] !== target[i]) return false;
    }
    return true;
}
