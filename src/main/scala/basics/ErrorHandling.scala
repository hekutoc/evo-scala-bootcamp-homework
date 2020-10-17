package main.scala.error_handling

import scala.util.Try

object ErrorHandling {
  import cats.data.ValidatedNec
  import cats.syntax.all._



  object Homework {

    case class PaymentCard(name: String, number: String, expirationDate: String, securityCode: String)

    sealed trait ValidationError
    object ValidationError {
      final case object NameLengthIsInvalid extends ValidationError {
        override def toString: String = "Name must not be empty"
      }
      final case object NumberNonNumeric extends ValidationError {
        override def toString: String = "Card number must contain digits only"
      }
      final case object NumberInvalidLength extends ValidationError {
        override def toString: String = "Card number must contain 16 digits"
      }
      final case object NumberChecksumNotCorrect extends ValidationError {
        override def toString: String = "Card number is not valid"
      }

      final case object ExpirationFormatNotValid extends ValidationError {
        override def toString: String = "Card expiration must meet format mm/yy"
      }
      final case object ExpirationYearNotValid extends ValidationError {
        override def toString: String = "Card expiration year is not valid"
      }
      final case object ExpirationMonthNotValid extends ValidationError {
        override def toString: String = "Card expiration month is not valid"
      }
      final case object SecurityCodeFormatNotValid extends ValidationError {
        override def toString: String = "Security code format is not valid"
      }


    }

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]


    object ValidationUtils {
      def validNumeric(str: String, errorToRaise: ValidationError) : AllErrorsOr[String] = {
        if (str.forall(_.isDigit)) str.validNec
        else errorToRaise.invalidNec
      }

      def validLength(str: String, length: Int, errorToRaise: ValidationError): AllErrorsOr[String] = {
        if(str.length == length) str.validNec
        else errorToRaise.invalidNec
      }

      def validNum(string: String, min: Int, max: Int, errorToRaise: ValidationError): AllErrorsOr[String] = {
        string.toIntOption match {
          case Some(num) if (num>=min & num <= max) => string.validNec
          case _  => errorToRaise.invalidNec
        }
      }
    }

    object PaymentCardValidator {
      import ValidationError._

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      def validateName(name: String): AllErrorsOr[String] =
        if (name.trim.length > 0) name.validNec
        else NameLengthIsInvalid.invalidNec



      def validateNumber(number: String): AllErrorsOr[String] = {
        def validChecksum(number: String): AllErrorsOr[String] = {
          Try({
            val digits = number.map(_.toString.toInt)
            digits.take(15).foldLeft[Int](0)(_+_%10) == digits.last
          }).toOption match {
            case Some(bool) if bool => number.validNec
            case _ => NumberChecksumNotCorrect.invalidNec
          }
        }

        def validLength(number: String): AllErrorsOr[String] = {
          ValidationUtils.validLength(number, 16, NumberInvalidLength)
        }

        ValidationUtils.validNumeric(number, NumberNonNumeric) productR validLength(number) productR validChecksum(number)
      }

      def validateExpirationDate(date: String): AllErrorsOr[String] = {
        def validMonth(string: String): AllErrorsOr[String] = ValidationUtils.validNum(string, 1, 12, ExpirationMonthNotValid)
        def validYear(string: String): AllErrorsOr[String] = ValidationUtils.validNum(string, 0, 99, ExpirationYearNotValid)

        val dateRegex = raw"(\\d{2})/(\\d{2})".r
        val err = ExpirationFormatNotValid
        date match {
          case dateRegex(month, year) =>
            ValidationUtils.validLength(month, 2, err)
              .productR(ValidationUtils.validLength(year, 2, err))
              .productR(validMonth(month))
              .productR(validYear(year))
          case _ => err.invalidNec
        }
      }

      def validateSecurityCode(string: String): AllErrorsOr[String] = {
        ValidationUtils.validNumeric(string, SecurityCodeFormatNotValid)
          .productR(ValidationUtils.validLength(string, 3, SecurityCodeFormatNotValid))
      }

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[PaymentCard] = {
        (validateName(name),
          validateNumber(number),
          validateExpirationDate(expirationDate),
          validateSecurityCode(securityCode)).mapN(PaymentCard)
      }
    }
  }
}
