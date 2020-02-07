
export function mapAtIndex<T>(array: T[], index: number, f: ((t: T) => T)): T[] {
    return [...array.slice(0, index), f(array[index]), ...array.slice(index + 1)]
}
