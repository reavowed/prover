package net.prover.controllers

import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

import javax.servlet.http.HttpServletResponse

@RestController
@RequestMapping(Array("/"))
class MainController  {
  @GetMapping(value = Array(""))
  def get(response: HttpServletResponse) = response.sendRedirect("/books")
}
