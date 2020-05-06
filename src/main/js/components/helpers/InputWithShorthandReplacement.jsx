import _ from "lodash";
import React, {useContext} from "react";
import EntryContext from "../EntryContext";
import PrettifiedAutosuggestOnIndividualWords from "./PrettifiedAutosuggestOnIndividualWords";

export default function InputWithShorthandReplacement({value, onChange, ...inputProps}) {
  const context = useContext(EntryContext);

  function replaceCompletedWord(word) {
    return _.has(context.definitionShorthands, word) ?
      context.definitionShorthands[word].serialized :
      word;
  }

  function matchWords(words, searchText) {
    if (!searchText.length) {
      return true;
    }
    if (!words.length) {
      return false;
    }
    const [firstWord, ...remainingWords] = words;
    if (!firstWord.length) {
      return matchWords(remainingWords, searchText);
    }
    if (firstWord[0].toUpperCase() === searchText[0].toUpperCase()) {
      return matchWords([firstWord.substring(1), ...remainingWords], searchText.substring(1)) ||
        matchWords(remainingWords, searchText.substring(1));
    } else {
      return matchWords(remainingWords, searchText);
    }
  }

  function isMatch(key, searchText) {
    const keyWords = key.split(/(?=[A-Z])/);
    return matchWords(keyWords, searchText);
  }

  function getSuggestions(lastWord) {
    if (lastWord.length > 1) {
      const matchingDefinitions = _.chain(context.definitions)
        .filter((value, key) => isMatch(key, lastWord))
        .map((value) => value.symbol.serialized)
        .value();
      const matchingShorthands = _.chain(context.definitionShorthands)
        .filter((value, key) => isMatch(key, lastWord))
        .map(symbol => symbol.serialized)
        .value();
      const matchingTypes = _.chain(context.typeDefinitions)
        .flatMap((value, key) => [key, ..._.values(value.properties).map(p => p.symbol)])
        .filter(key => isMatch(key, lastWord))
        .value();
      return _.chain([...matchingDefinitions, ...matchingShorthands, ...matchingTypes]).uniq().sortBy().value().slice(0, 10);
    } else {
      return [];
    }
  }

  return <PrettifiedAutosuggestOnIndividualWords value={value}
                                                 onChange={onChange}
                                                 getSuggestions={getSuggestions}
                                                 replaceCompletedWord={replaceCompletedWord}
                                                 inputProps={inputProps}/>;
}


