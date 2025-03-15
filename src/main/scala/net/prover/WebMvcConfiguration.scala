package net.prover

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.springframework.context.annotation.{Bean, Configuration}

@Configuration
class WebMvcConfiguration {
  @Bean
  def objectMapper: ObjectMapper = {
    JsonMapper.builder()
      .addModule(DefaultScalaModule)
      .build()
  }
}
