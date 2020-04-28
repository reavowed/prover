import _ from "lodash";
import React, {useCallback, useContext, useState} from "react";
import EntryContext from "../../EntryContext";
import PrettifiedAutosuggest from "../../helpers/PrettifiedAutosuggest";

export default function TypeQualifierAutocomplete({value, onChange, typeSymbol}) {
  const entryContext = useContext(EntryContext);
  const [suggestions, setSuggestions] = useState([]);

  const qualifiers = _.find(entryContext.typeDefinitions, d => d.symbol === typeSymbol)?.qualifiers ?? [];

  const fetchSuggestions = useCallback(
    ({value}) => {
      setSuggestions(_.chain(qualifiers).map(q => q.symbol).filter(s => _.startsWith(s, value)).value());
    },
    [value, qualifiers]);

  return <PrettifiedAutosuggest suggestions={suggestions}
                                onSuggestionsFetchRequested={fetchSuggestions}
                                onSuggestionsClearRequested={() => setSuggestions([])}
                                shouldRenderSuggestions={() => true}
                                getSuggestionValue={s => s}
                                inputProps={{value, onChange: (event, {newValue}) => onChange(newValue), className:"form-control"}} />
}
