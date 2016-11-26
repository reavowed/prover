package net.prover

import java.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.context.annotation.Configuration
import org.springframework.http.converter.HttpMessageConverter
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter

@SpringBootApplication
class Application

@Configuration
class WebMvcConfiguration extends WebMvcConfigurerAdapter {
  override def configureMessageConverters(converters: util.List[HttpMessageConverter[_]]): Unit = {
    // Configure Jackson to serialize Scala classes
    val objectMapper = new ObjectMapper()
    objectMapper.registerModule(DefaultScalaModule)
    converters.add(new MappingJackson2HttpMessageConverter(objectMapper))
    super.configureMessageConverters(converters)
  }
}

object Application {
  def main(args: Array[String]): Unit = {
    SpringApplication.run(classOf[Application], args: _*)
  }
}
