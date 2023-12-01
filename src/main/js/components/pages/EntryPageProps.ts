import {BinaryRelation} from "../definitions/BinaryRelation";
import {LinkSummary} from "../definitions/LinkSummary";
import {ChapterUsages} from "./components/Usages";

export type EntryPageProps = {
  url: string
  bookLink: LinkSummary
  chapterLink: LinkSummary
  previous?: LinkSummary
  next?: LinkSummary
  usages: ChapterUsages[]
  binaryRelations: BinaryRelation[]
}
