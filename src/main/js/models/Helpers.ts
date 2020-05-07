
export function mapAtIndex<T>(array: T[], index: number, f: ((t: T) => T)): T[] {
    return [...array.slice(0, index), f(array[index]), ...array.slice(index + 1)]
}

export function mapAtIndexWithMetadata<T, S>(array: T[], index: number, f: ((t: T) => [T, S])): [T[], S] {
    const [newElement, metadata] = f(array[index]);
    return [[...array.slice(0, index), newElement, ...array.slice(index + 1)], metadata];
}

export function flatMapAtIndex<T>(array: T[], index: number, f: ((t: T) => T[])): T[] {
    return [...array.slice(0, index), ...f(array[index]), ...array.slice(index + 1)]
}

export function replaceAtIndex<T>(array: T[], index: number, newValue: T): T[] {
    return [...array.slice(0, index), newValue, ...array.slice(index + 1)]
}
