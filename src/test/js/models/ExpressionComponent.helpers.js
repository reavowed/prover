import * as _ from "lodash";

export function treeToString(tree) {
  if (_.isString(tree)) {
    return tree;
  } else {
    return _.map(tree.children, treeToString).join("")
  }
}

export function getWords(tree) {
  return _.filter(tree.children, (c, i) => i % 2 === 0);
}
