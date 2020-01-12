import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import ChapterContext from "./ChapterContext";

export default function DeleteEntryButton({entry}) {
  const context = useContext(ChapterContext);
  return <Button size="sm" variant="danger" className="ml-1 py-0" onClick={() => context.updateChapter(entry.url, {method: "DELETE"})}><span className="fas fa-ban"/></Button>
}
