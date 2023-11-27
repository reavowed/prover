export function isDefined<T>(t: T | undefined | null): t is T {
    return !!t
}
