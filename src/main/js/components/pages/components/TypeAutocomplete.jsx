import _ from "lodash";
import React, {useCallback, useContext, useState} from "react";
import EntryContext from "../../EntryContext";
import PrettifiedAutosuggest from "../../helpers/PrettifiedAutosuggest";

export default function TypeAutocomplete({value, onChange}) {
  const entryContext = useContext(EntryContext);
  const [suggestions, setSuggestions] = useState([]);

  const fetchSuggestions = useCallback(
    ({value}) => {
      setSuggestions(_.chain(entryContext.typeDefinitions).map(d => d.symbol).filter(s => _.startsWith(s, value)).value());
    },
    [value]);

  return <PrettifiedAutosuggest suggestions={suggestions}
                                onSuggestionsFetchRequested={fetchSuggestions}
                                onSuggestionsClearRequested={() => setSuggestions([])}
                                shouldRenderSuggestions={() => true}
                                getSuggestionValue={s => s}
                                inputProps={{value, onChange: (event, {newValue}) => onChange(newValue), className:"form-control"}} />
}
