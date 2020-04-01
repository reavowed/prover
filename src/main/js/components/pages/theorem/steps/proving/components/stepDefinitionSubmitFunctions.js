export function createSubmitFunctionForStepDefinitionEndpointFromInference(contextFunction, path, endpointName, method, onError, onCancel) {
  return (possibleInference, possibleTarget, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    let promise =  contextFunction(path, endpointName, {
      method,
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds,
        wrappingSymbols: possibleTarget ? possibleTarget.wrappingDefinitions : [],
        serializedIntendedPremiseStatements: premiseStatements.map(p => p.serialize()),
        serializedIntendedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: possibleConclusion.additionalVariableNames
      }
    })
    if (onCancel) {
      promise = promise.then(onCancel);
    }
    return promise.catch(onError);
  };
}

export function createSubmitFunctionForStepDefinitionEndpointFromPremise(contextFunction, path, endpointName, method, onError, onCancel) {
  return (premiseStatement, substitutions, selectedConclusion, premiseStatements, conclusionStatement) => {
    let promise = contextFunction(path, endpointName, {
      method,
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds: selectedConclusion.extractionInferenceIds,
        wrappingSymbols: [],
        serializedIntendedPremiseStatements: premiseStatements.map(p => p.serialize()),
        serializedIntendedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: selectedConclusion.additionalVariableNames
      }
    });
    if (onCancel) {
      promise = promise.then(onCancel);
    }
    return promise.catch(onError);
  };
}
