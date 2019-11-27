package net.prover

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
  override def configureMessageConverters(converters: java.util.List[HttpMessageConverter[_]]): Unit = {
    // Configure Jackson to serialize Scala classes
    converters.add(new MappingJackson2HttpMessageConverter(JsonMapping.objectMapper))
    super.configureMessageConverters(converters)
  }
}

object Application {
  def main(args: Array[String]): Unit = {
    SpringApplication.run(classOf[Application], args: _*)
  }
}
