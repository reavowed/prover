import path from "path";
import {combineReducers} from "redux";
import {Expression} from "../../models/Expression";
import {Step} from "../../models/Step";

export const parseTheorem = theoremJson => {
  return {
    name: theoremJson.name,
    id: theoremJson.id,
    key: theoremJson.key,
    premises: theoremJson.premises.map(Expression.parseFromJson),
    conclusion: Expression.parseFromJson(theoremJson.conclusion),
    proofs: theoremJson.proofs.map(proof => Step.parseFromJson(proof.steps))
  };
};

export const UpdateTheorem = function({newInferences, theorem}) {
  return {
    type: "UpdateTheorem",
    newTheorem: parseTheorem(theorem),
    newInferences
  }
};

export const SetHighlightedPremises = function(newHighlightedPremises) {
  return {
    type: "SetHighlightedPremises",
    newHighlightedPremises
  }
};

export const SetHighlightedConclusion = function(newHighlightedConclusion) {
  return {
    type: "SetHighlightedConclusion",
    newHighlightedConclusion
  }
};

export const FetchJson = function(subpath, options) {
  return (dispatch, getState) => {
    return window.fetch(path.join(getState().url, subpath), options)
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      })
  }
};
export const FetchJsonAndUpdate = (subpath, options) => {
  return (dispatch) => {
    return dispatch(FetchJson(subpath, options)).then(newTheorem => dispatch(UpdateTheorem(newTheorem)))
  }
};

export const FetchJsonForStep = (proofIndex, stepPath, childPath, options) => {
  const combinedPath = path.join("proofs", proofIndex.toString(), stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
  return FetchJson(combinedPath, options);
};
export const FetchJsonForStepAndUpdate = (proofIndex, stepPath, childPath, options) => {
  return (dispatch) => {
    return dispatch(FetchJsonForStep(proofIndex, stepPath, childPath, options)).then(newTheorem => dispatch(UpdateTheorem(newTheorem)))
  }
};

const highlightedPremisesReducer = function(state = [], action) {
  switch (action.type) {
    case "SetHighlightedPremises":
      return action.newHighlightedPremises;
    default:
      return state;
  }
};

const highlightedConclusionReducer = function(state = [], action) {
  switch (action.type) {
    case "SetHighlightedConclusion":
      return action.newHighlightedConclusion;
    default:
      return state;
  }
};

const theoremReducer = function(state = {}, action) {
  switch (action.type) {
    case "UpdateTheorem":
      _.merge(window.inferences, action.newInferences);
      return action.newTheorem;
    default:
      return state;
  }
};

const urlReducer = function(state = "", action) {
  return state;
};

export default combineReducers({
  theorem: theoremReducer,
  highlightedPremises: highlightedPremisesReducer,
  highlightedConclusion: highlightedConclusionReducer,
  url: urlReducer
});
