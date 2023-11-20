package net.prover.proving.extraction

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import net.prover.JsonMapping
import net.prover.model.Inference
import net.prover.model.definitions.Equality
import net.prover.util.Direction
import org.springframework.core.convert.converter.Converter
import org.springframework.stereotype.Component

case class ExtractionDefinition(extractionInferences: Seq[Inference.Summary]) {
  def serialized: ExtractionDefinition.Serialized = {
    ExtractionDefinition.Serialized(extractionInferences.map(_.id))
  }
  def matches(serializedDefinition: ExtractionDefinition.Serialized): Boolean = {
    extractionInferences.map(_.id) == serializedDefinition.extractionInferenceIds
  }
  def addInitialExtractionInference(inference: Inference.Summary): ExtractionDefinition = {
    copy(extractionInferences = inference +: extractionInferences)
  }
}

object ExtractionDefinition {
  val Empty: ExtractionDefinition = ExtractionDefinition(Nil)

  case class Serialized @JsonCreator() (@JsonProperty("extractionInferenceIds") extractionInferenceIds: Seq[String]) {
    def depth: Int = extractionInferenceIds.length
    def reverseIfNecessary(direction: Direction, equality: Equality): Serialized = {
      if (!direction.isReversed) {
        this
      } else if (extractionInferenceIds.lastOption.contains(equality.reversal.inference.id)) {
        copy(extractionInferenceIds = extractionInferenceIds.init)
      } else {
        copy(extractionInferenceIds = extractionInferenceIds :+ equality.reversal.inference.id)
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
