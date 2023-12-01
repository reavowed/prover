import React from "react";
import _ from "lodash";

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

type JsonPrimitive = string | number | boolean | null
interface JsonMap extends Record<string, JsonPrimitive | JsonArray | JsonMap> {}
interface JsonArray extends Array<JsonPrimitive | JsonArray | JsonMap> {}
export type Json = JsonPrimitive | JsonMap | JsonArray
export type JsonRequestInit = Omit<RequestInit, "body"> & {
    body?: Json
}

export function fetchJson(input: RequestInfo | URL, init: JsonRequestInit | undefined): Promise<any> {
  const {body, headers, ...otherInit} = init || {};
  return window.fetch(input, {
    ...otherInit,
    body: !_.isUndefined(body) ? JSON.stringify(body) : null,
    headers: {"Content-Type": "application/json", ...(headers || {})}
  })
    .then(response => new Promise<any>(((resolve, reject) => {
        const method = response.ok ? resolve : reject;
        return response.headers.get("Content-Length") === "0" ? method(null) : response.json().then(method);
      }))
    )
}
