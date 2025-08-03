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
  const {body: rawBody, headers, ...otherInit} = init || {};
  const body = (_.isArray(rawBody) || _.isObject(rawBody)) ? JSON.stringify(rawBody) :
      rawBody?.toString();
  return window.fetch(input, {
    ...otherInit,
    body,
    headers: {"Content-Type": "application/json", ...(headers || {})}
  })
    .then(response => new Promise<any>(((resolve, reject) => {
        if (response.ok) {
            return response.headers.get("Content-Length") === "0" ?
                resolve(null) :
                response.json().then(resolve, reject);
        } else {
            response.text().then(reject, reject);
        }
      }))
    )
}
