package net.prover.proving.extraction

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import net.prover.JsonMapping
import net.prover.model.Inference
import net.prover.model.definitions.Equality
import net.prover.util.Direction
import org.springframework.core.convert.converter.Converter
import org.springframework.stereotype.Component

case class ExtractionDefinition(
  extractionInferences: Seq[Inference.Summary],
  rewriteInference: Option[Inference.Summary])
{
  def serialized: ExtractionDefinition.Serialized = {
    ExtractionDefinition.Serialized(extractionInferences.map(_.id), rewriteInference.map(_.id))
  }
  def matches(serializedDefinition: ExtractionDefinition.Serialized): Boolean = {
    extractionInferences.map(_.id) == serializedDefinition.extractionInferenceIds &&
      rewriteInference.map(_.id) == serializedDefinition.rewriteInferenceId
  }
  def addNextExtractionInference(inference: Inference.Summary): ExtractionDefinition = {
    copy(extractionInferences = extractionInferences :+ inference)
  }
  def setRewriteInference(inference: Inference.Summary): ExtractionDefinition = {
    copy(rewriteInference = Some(inference))
  }
}

object ExtractionDefinition {
  val Empty: ExtractionDefinition = ExtractionDefinition(Nil, None)

  case class Serialized @JsonCreator() (@JsonProperty("extractionInferenceIds") extractionInferenceIds: Seq[String], rewriteInferenceId: Option[String]) {
    def depth: Int = extractionInferenceIds.length
    def reverseIfNecessary(direction: Direction, equality: Equality): Serialized = {
      if (!direction.isReversed) {
        this
      } else if (rewriteInferenceId.contains(equality.reversal.inference.id)) {
        copy(rewriteInferenceId = None)
      } else {
        copy(rewriteInferenceId = Some(equality.reversal.inference.id))
      }
    }
  }
}

@Component
class ExtractionDefinitionConverter extends Converter[String, ExtractionDefinition.Serialized] {
  override def convert(source: String): ExtractionDefinition.Serialized = {
    JsonMapping.objectMapper.readValue(source, classOf[ExtractionDefinition.Serialized])
  }
}
