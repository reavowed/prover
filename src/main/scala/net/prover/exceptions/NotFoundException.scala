package net.prover.exceptions

case class NotFoundException(objectDescription: String) extends Exception(s"$objectDescription not found")
