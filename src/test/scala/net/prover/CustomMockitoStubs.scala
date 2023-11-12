package net.prover

import org.mockito.internal.debugging.VerboseMockInvocationLogger
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{OngoingStubbing, Stubber}
import org.specs2.control.ImplicitParameters.ImplicitParam
import org.specs2.control.{Property, Use}
import org.specs2.mock.MockitoMocker
import org.specs2.mock.mockito.{IgnoreStubs, MockitoStubs, MocksCreation}
import org.specs2.reflect.ClassesOf

import scala.reflect.ClassTag

/**
 * This trait provides functionalities to declare stub values on method calls.
 *
 * Usage:
 * {{{
 * mockedList.get(0) returns "one"
 * mockedList.get(0) returns ("one", "two")
 * mockedList.get(0) throws new Exception("unexpected")
 * mockedList.get(0) answers ( i => "value " + i.toString )
 * mockedList.get(any) responds { case i: Int => (i + 1).toString }
 * }}}
 *
 * It is also possible to chain stubs like this:
 * {{{
 * mockedList.get(0) returns "one" thenReturns "two"
 * mockedList.get(0) returns "one" thenThrows new Exception("unexpected now")
 * }}}
 */
trait CustomMockitoStubs {
  private val mocker = new MockitoMocker {}

  def mock[T: ClassTag]: T = mocker.mock(implicitly[ClassTag[T]])

  /**
    * Identical to the implementation in MockitoStubs, but with the parameter passed by reference.
    * This allows things like `foo.bar() returns createMock()`, where createMock() itself carries
    * out stubbing.
    */
  class CustomStubbed[T](c: =>T) {
    def returns(t: T, t2: T*): OngoingStubbing[T] = {
      if (t2.isEmpty)
        mocker.when(c).thenReturn(t)
      else
        t2.foldLeft (mocker.when(c).thenReturn(t)) { (res, cur) => res.thenReturn(cur) }
    }
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new MockAnswer(function))
    def answers(function: (Any, Any) => T) = mocker.when(c).thenAnswer(new MockAnswer2(function))
    def answers(function: Array[AnyRef] => T)(implicit p: ImplicitParam) = Use.ignoring(p) {
      mocker.when(c).thenAnswer(new MockAnswer3(function))
    }
    def responds(function: Any => T) = answers(function)
    def throws[E <: Throwable](e: E*): OngoingStubbing[T] = {
      if (e.isEmpty) throw new java.lang.IllegalArgumentException("The parameter passed to throws must not be empty")
      e.drop(1).foldLeft(mocker.when(c).thenThrow(e.head)) { (res, cur) => res.thenThrow(cur) }
    }
  }

  /**
    * Likewise, has the argument passed by reference.
    */
  implicit def theCustomStubbed[T](c: => T) = new CustomStubbed(c)

  class MockAnswer[T](function: Any => T) extends Answer[T] {
    def answer(invocation: InvocationOnMock): T = {
      val args = invocation.getArguments
      if (args.size == 0) function match {
        case f: Function0[_] => f().asInstanceOf[T]
        case f: Function1[_, _] => f(invocation.getMock)
      }
      else if (args.size == 1) function(args(0))
      else function(args)
    }
  }

  class MockAnswer2[T](function: (Any, Any) => T) extends Answer[T] {
    def answer(invocation: InvocationOnMock): T = function(invocation.getArguments, invocation.getMock)
  }

  class MockAnswer3[T](function: Array[AnyRef] => T) extends Answer[T] {
    def answer(invocation: InvocationOnMock): T =
      function(invocation.getArguments)
  }
}
