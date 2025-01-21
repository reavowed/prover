package net.prover.proving.extraction

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import net.prover.JsonMapping
import net.prover.model.Inference
import net.prover.proving.structure.inferences.Equality
import net.prover.util.Direction
import org.springframework.core.convert.converter.Converter
import org.springframework.stereotype.Component

case class ExtractionDefinition(
  extractionInferences: Seq[Inference.Summary],
  reversalInference: Option[Inference.Summary],
  leftRewrite: Option[Inference.Summary] = None,
  rightRewrite: Option[Inference.Summary] = None)
{
  def serialized: ExtractionDefinition.Serialized = {
    ExtractionDefinition.Serialized(extractionInferences.map(_.id), reversalInference.map(_.id))
  }
  def matches(serializedDefinition: ExtractionDefinition.Serialized): Boolean = {
    extractionInferences.map(_.id) == serializedDefinition.extractionInferenceIds &&
      reversalInference.map(_.id) == serializedDefinition.reversalInferenceId
  }
  def addNextExtractionInference(inference: Inference.Summary): ExtractionDefinition = {
    copy(extractionInferences = extractionInferences :+ inference)
  }
  def setReversalInference(inference: Inference.Summary): ExtractionDefinition = {
    copy(reversalInference = Some(inference))
  }
}

object ExtractionDefinition {
  val Empty: ExtractionDefinition = ExtractionDefinition(Nil, None, None, None)

  case class Serialized @JsonCreator() (@JsonProperty("extractionInferenceIds") extractionInferenceIds: Seq[String], reversalInferenceId: Option[String]) {
    def depth: Int = extractionInferenceIds.length
    def reverseIfNecessary(direction: Direction, equality: Equality): Serialized = {
      if (!direction.isReversed) {
        this
      } else if (reversalInferenceId.nonEmpty) {
        copy(reversalInferenceId = None)
      } else {
        copy(reversalInferenceId = Some(equality.reversal.inference.id))
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
