package net.prover

import org.hamcrest.{BaseMatcher, Description}
import org.mockito.InOrder
import org.mockito.hamcrest.MockitoHamcrest
import org.mockito.verification.VerificationMode
import org.specs2.control.Exceptions.catchAll
import org.specs2.execute.{Failure, Result, Success}
import org.specs2.matcher.*

import scala.reflect.ClassTag

trait MockitoHelpers extends ThrownExpectationsCreation with ThrownStandardResults {
  def mock[T : ClassTag as classTag]: T = org.mockito.Mockito.mock(classTag.runtimeClass.asInstanceOf[Class[T]])

  def any[T : ClassTag as classTag]: T = org.mockito.ArgumentMatchers.any(classTag.runtimeClass.asInstanceOf[Class[T]])

  extension [T] (t: => T)
    def returns(result: T): Unit = {
      org.mockito.Mockito.when(t).thenReturn(result)
    }

  def there = new Calls

  class Calls {
    def was[T](calls: => T): Result = {
      createExpectable(calls).applyMatcher(checkCalls)
    }
  }

  private def checkCalls[T]: Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]): Result = {
      catchAll { s.value } { identity } match {
        case Right(v) =>
          success("The mock was called as expected")
        case Left(e: AssertionError) =>
          failure("The mock was not called as expected: " + e.getMessage)
        // unexpected error from inside Mockito itself
        case Left(e) => throw e
      }
    }
  }

  /** one call only made to the mock */
  def one[T <: AnyRef](mock: T): T = org.mockito.Mockito.verify(mock, org.mockito.Mockito.times(1))

  given functionConversion[T, S]: Conversion[(T, Matcher[S]), (T => S)] = { values =>
    MockitoHamcrest.argThat(
      HamcrestMatcherAdapter(
        values._2 ^^ { (f: (T => S)) => f(values._1)}))
  }

  /**
   * Adapter class to use specs2 matchers as Hamcrest matchers
   */
  case class HamcrestMatcherAdapter[T](m: Matcher[T]) extends BaseMatcher[T] {
    /** this variable is necessary to store the result of a match */
    private var message = ""

    def matches(item: Object): Boolean = matchesSafely(item: Any, m.asInstanceOf[Matcher[Any]])

    /** this method used to be called when extending TypeSafeMatcher. However the final `matches` method was bypassing 'null' values */
    def matchesSafely(item: T): Boolean = matchesSafely(item, m)

    private def matchesSafely[A](item: A, matcher: Matcher[A]): Boolean = {
      // special case for by-name arguments
      // in that case we apply the Function0 to get the value
      val i = if (item != null && item.isInstanceOf[Function0[_]]) item.asInstanceOf[Function0[_]].apply().asInstanceOf[A] else item
      try {
        matcher.apply(createExpectable(i)) match {
          case f: Failure => message = f.message; false
          case _ => true
        }
        // a class cast exception can happen if we tried: vet.treat(dog); there must be one(vet).treat(bird) (see issue #222)
      } catch {
        case c: ClassCastException => false
        case e: Throwable =>
          // this is a stop-gap solution for #584
          // it seems that Mockito can pass null values in some cases
          if (item == null) false
          else throw e
      }
    }

    def describeTo(description: Description): Unit = {
      description.appendText(message)
      ()
    }
  }


}
