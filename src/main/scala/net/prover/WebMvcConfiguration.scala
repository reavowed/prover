package net.prover

import org.springframework.context.annotation.Configuration
import org.springframework.http.converter.HttpMessageConverter
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter
import org.springframework.web.servlet.config.annotation.{PathMatchConfigurer, WebMvcConfigurerAdapter}

@Configuration
class WebMvcConfiguration extends WebMvcConfigurerAdapter {
  override def configureMessageConverters(converters: java.util.List[HttpMessageConverter[_]]): Unit = {
    // Configure Jackson to serialize Scala classes
    converters.add(new MappingJackson2HttpMessageConverter(JsonMapping.objectMapper))
    super.configureMessageConverters(converters)
  }

  override def configurePathMatch(configurer: PathMatchConfigurer): Unit = {
    super.configurePathMatch(configurer)
    configurer.setUseSuffixPatternMatch(false)
  }
}
