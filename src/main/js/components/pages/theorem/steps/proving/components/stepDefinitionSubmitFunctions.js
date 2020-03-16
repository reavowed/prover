export function createSubmitFunctionForStepDefinitionEndpointFromInference(context, path, endpointName, method, onCancel, onError) {
  return (possibleInference, possibleTarget, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, endpointName, {
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
      .then(onCancel)
      .catch(onError);
  };
}

export function createSubmitFunctionForStepDefinitionEndpointFromPremise(context, path, endpointName, method, onCancel, onError) {
  return (premiseStatement, substitutions, selectedConclusion, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, endpointName, {
      method,
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds,
        wrappingSymbols: [],
        serializedIntendedPremiseStatements: premiseStatements.map(p => p.serialize()),
        serializedIntendedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: selectedConclusion.additionalVariableNames
      }
    })
      .then(onCancel)
      .catch(onError);
  };
}
