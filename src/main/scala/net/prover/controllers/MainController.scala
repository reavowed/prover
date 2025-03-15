package net.prover.controllers

import jakarta.servlet.http.HttpServletResponse
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/"))
class MainController  {
  @GetMapping(value = Array(""))
  def get(response: HttpServletResponse) = response.sendRedirect("/books")
}
