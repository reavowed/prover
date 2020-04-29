import React from "react";
import {Usages} from "./Usages";

export default function StatementDefinitionUsages({usages, statementDefinition}) {
  return <>
    <Usages.ForInference usages={usages} inferenceId={statementDefinition.constructionInference.id} title="Construction" />
    <Usages.ForInference usages={usages} inferenceId={statementDefinition.deconstructionInference.id} title="Deconstruction" />
  </>;
}
