export function createSubmitFunctionForStepDefinitionEndpointFromInference(contextFunction, path, endpointName, method, onError) {
  return (possibleInference, possibleTarget, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return contextFunction(path, endpointName, {
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
      .catch(onError);
  };
}

export function createSubmitFunctionForStepDefinitionEndpointFromPremise(contextFunction, path, endpointName, method, onError) {
  return (premiseStatement, substitutions, selectedConclusion, premiseStatements, conclusionStatement) => {
    return contextFunction(path, endpointName, {
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
    })
      .catch(onError);
  };
}
