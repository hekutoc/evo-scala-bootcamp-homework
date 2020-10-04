package main.scala.basics

import scala.Left
import scala.io.Source

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  final case class DivisionResult(dividend: Double, divisor: Double, result: Double) extends Result

  final case class AverageResult(value: List[Double], d: Double) extends Result

  final case class MaxResult(value: List[Double], d: Double) extends Result

  final case class MinResult(value: List[Double], d: Double) extends Result

  final case class SumResult(value: List[Double], d: Double) extends Result


  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val args = x.split(" ").toList
    val cmd = args.headOption
    val numbers = args.tail.map(_.toDoubleOption)
    if (numbers.contains(None) && numbers.nonEmpty) {
      Left(ErrorMessage("Numeric arguments expected"))
    } else {
      val fNumbers = numbers.flatten
      cmd match {
        case None => Left(ErrorMessage("Command not found"))
        case Some("divide") =>
          if (fNumbers.length != 2) {
            Left(ErrorMessage("Expected 2 numeric arguments"))
          } else {
            Right(Command.Divide(fNumbers(0), fNumbers(1)))
          }

        case Some("sum") => Right(Command.Sum(fNumbers))
        case Some("min") => Right(Command.Min(fNumbers))
        case Some("max") => Right(Command.Max(fNumbers))
        case Some("average") => Right(Command.Average(fNumbers))
        case Some(other) => Left(ErrorMessage(if(other.length > 0) s"Command '$other' is not supported" else "Command is not provided"))
      }
    }
    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend, divisor) => {
        if (divisor == 0) Left(ErrorMessage("Division by zero"))
        else Right(DivisionResult(dividend, divisor, dividend / divisor))
      }
      case Command.Average(numbers) => Right(AverageResult(numbers, numbers.sum / numbers.size))
      case Command.Sum(numbers) => Right(SumResult(numbers, numbers.sum))
      case Command.Min(numbers) => Right(MinResult(numbers, numbers.min))
      case Command.Max(numbers) => Right(MaxResult(numbers, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    def pNum(number: Double) = number.toString.replaceAll("\\.?0+$", "")

    def numList(numbers: List[Double]) = numbers.map(pNum).mkString(" ")

    x match {
      case DivisionResult(dividend, divisor, result) => s"${pNum(dividend)} divided by ${pNum(divisor)} is ${pNum(result)}"
      case AverageResult(numbers, result) => s"the average of ${numList(numbers)} is ${pNum(result)}"
      case MaxResult(numbers, result) => s"the max of ${numList(numbers)} is ${pNum(result)}"
      case MinResult(numbers, result) => s"the min of ${numList(numbers)} is ${pNum(result)}"
      case SumResult(numbers, result) => s"the sum of ${numList(numbers)} is ${pNum(result)}"
    }
  }

  def process(x: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
    val out = for {
      cmd <- parseCommand(x)
      res <- calculate(cmd)
    } yield renderResult(res)

    out.fold(err => s"Error: ${err.value}", out => out)
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
