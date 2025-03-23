package net.prover

import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.context.ConfigurableApplicationContext

@SpringBootApplication
class Application

object Application {
  final def main(args: Array[String]): Unit = {
    SpringApplication.run(classOf[Application], args *)
  }
}
