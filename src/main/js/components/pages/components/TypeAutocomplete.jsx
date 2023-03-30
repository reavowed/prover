import _ from "lodash";
import React, {useCallback, useContext, useState} from "react";
import AvailableEntries from "../../AvailableEntries";
import PrettifiedAutosuggest from "../../helpers/PrettifiedAutosuggest";

export default function TypeAutocomplete({value, onChange}) {
  const availableEntries = useContext(AvailableEntries);
  const [suggestions, setSuggestions] = useState([]);

  const fetchSuggestions = useCallback(
    ({value}) => {
      setSuggestions(_.chain(availableEntries.typeDefinitions).map(d => d.symbol).filter(s => _.startsWith(s, value)).value());
    },
    [value]);

  return <PrettifiedAutosuggest suggestions={suggestions}
                                onSuggestionsFetchRequested={fetchSuggestions}
                                onSuggestionsClearRequested={() => setSuggestions([])}
                                shouldRenderSuggestions={() => true}
                                getSuggestionValue={s => s}
                                inputProps={{value, onChange: (event, {newValue}) => onChange(newValue), className:"form-control"}} />
}
