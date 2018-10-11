package net.prover.controllers

import javax.servlet.http.HttpServletResponse
import net.prover.services.BookService
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/"))
class MainController @Autowired() (bookService: BookService)  {
  @GetMapping(value = Array(""))
  def get(response: HttpServletResponse) = response.sendRedirect("/books")

  @GetMapping(value = Array("shorthands"), produces = Array("application/json;charset=UTF-8"))
  def getShorthands(): Map[String, String] = {
    bookService.books
      .flatMap(_.chapters)
      .flatMap(_.definitions)
      .flatMap(d => d.shorthand.toSeq.map(_ -> d.key.value))
      .toMap
  }
}
