package io.chrisdavenport.freeapfun

// import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import cats.arrow._
import cats.free._
// import scala.concurrent.ExecutionContext

object FreeApFunOne {
  sealed abstract class ValidationOp[A]
  case class Size(size: Int) extends ValidationOp[Boolean]
  case object HasNumber extends ValidationOp[Boolean]

  type Validation[A] = FreeApplicative[ValidationOp, A]
  def size(size: Int) : Validation[Boolean] = FreeApplicative.lift(Size(size))
  def hasNumber: Validation[Boolean] = FreeApplicative.lift(HasNumber)

  val prog : Validation[Boolean] = (size(5), hasNumber).mapN{
    case (l, r) => l && r
  }

  val compiler = new FunctionK[ValidationOp, Function1[String, ?]]{
    def apply[A](fa: ValidationOp[A]): Function1[String, A] = str => 
      fa match {
        case Size(size) => str.size >= size
        case HasNumber => str.exists(c => "0123456789".contains(c))
      }
  }

  val validator = prog.foldMap[Function1[String,?]](compiler)

  def testValid1(): Unit = {
    val validated1 = validator("1234")
    val validated2 = validator("12345")
    println(s"Validated1 $validated1")
    println(s"Validated2 $validated2")
  }

  val parCompiler = new FunctionK[ValidationOp, Kleisli[IO, String, ?]] {
    def apply[A](fa: ValidationOp[A]): Kleisli[IO, String, A] = Kleisli { str =>
      fa match {
        case Size(size) => IO { str.size >= size }
        case HasNumber => IO { str.exists(c => "0123456789".contains(c)) }
      }
    }
  }

  val logCompiler = new FunctionK[ValidationOp, Const[List[String], ?]] {
    def apply[A](fa: ValidationOp[A]): Const[List[String], A] = fa match {
      case Size(size) => Const(List(s"size >= $size"))
      case HasNumber => Const(List("has number"))
    }
  }

  def logValidation[A](validation: Validation[A]): List[String] = 
    validation.foldMap[Const[List[String], ?]](logCompiler).getConst

  def testValid2(): Unit = {
    val one = logValidation(prog)
    val two = logValidation(size(5) *> hasNumber *> size(10))
    val three = logValidation((hasNumber, size(3)).mapN(_ || _))
    println(one)
    println(two)
    println(three)
  }
}