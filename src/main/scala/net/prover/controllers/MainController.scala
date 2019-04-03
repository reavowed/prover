package net.prover.controllers

import javax.servlet.http.HttpServletResponse
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}
import net.prover.model._

@RestController
@RequestMapping(Array("/"))
class MainController @Autowired() (bookService: BookService)  {
  @GetMapping(value = Array(""))
  def get(response: HttpServletResponse) = response.sendRedirect("/books")
}
