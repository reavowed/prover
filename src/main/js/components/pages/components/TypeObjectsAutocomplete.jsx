import _ from "lodash";
import React, {useCallback, useContext} from "react";
import EntryContext from "../../EntryContext";
import PrettifiedAutosuggestOnIndividualWords from "../../helpers/PrettifiedAutosuggestOnIndividualWords";

export default function TypeObjectsAutocomplete({value, onChange, typeSymbol}) {
  const entryContext = useContext(EntryContext);
  const objects = _.find(entryContext.typeDefinitions, d => d.symbol === typeSymbol)?.relatedObjects ?? [];
  const getSuggestions = useCallback(
    (value) => _.chain(objects).map(q => q.symbol).filter(s => _.startsWith(s, value)).value(),
    [value, objects]);
  return <PrettifiedAutosuggestOnIndividualWords value={value}
                                                 onChange={onChange}
                                                 getSuggestions={getSuggestions}
                                                 inputProps={{className: "form-control"}}
                                                 shouldRenderSuggestions={() => true} />;
}
