package com.paidy

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.util.matching.Regex

object HomeTask extends App {

  /**
   * Write a function that takes an Integer and returns it as a string with the correct ordinal indicator suffix (in English). Examples: 1 => 1st, 2 => 2nd.
   * Write a function that takes two dates (date_from, date_to, in dd-mm-yyyy format) and returns the number of Sundays in that range. Example: (‘01-05-2021’, ‘30-05-2021’) => 5.
   * Mask personal information: create a function that takes a String as input and returns it partly obfuscated. The function only recognizes emails and phone numbers, any other String that doesn’t match these types results in an error.
   * Emails: emails need to be in a valid email format. To obfuscate it, it should be converted to lowercase and all characters in the local-part between the first and last should be replaced by 5 asterisks (*). Example: local-part@domain-name.com => l*****t@domain-name.com.
   * Phone numbers: a phone number consists of at least 9 digits (0-9) and may contain these two characters (‘ ‘, ‘+’) where ‘+’ is only accepted when is the first character. To obfuscate it, spaces (‘ ‘) are converted to dashes (‘-’), any digit is converted to an asterisk (‘*’) except for the last 4, which remain unchanged and the plus sign (‘+’) also remains unchanged (if present). Example: +44 123 456 789 => +**-***-**6-789.
   * Write a short paragraph about your proudest achievement. Can you describe it? What was your role in it? Why is it important to you and what have you learnt from it?
   */


  def toOrderString(number: Int): String = {
    val j = number % 10
    val k = number % 100
    (j, k) match {
      case (j, k) if j == 1 && k != 11 => s"${number}st"
      case (j, k) if j == 2 && k != 12 => s"${number}nd"
      case (j, k) if j == 3 && k != 13 => s"${number}rd"
      case _ => s"${number}th"
    }
  }

  def countNumberOfSundays(startDate: String, endDate: String): Int = {
    val formatterDateTime = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val fromDate = LocalDate.parse(startDate, formatterDateTime)
    val toDate = LocalDate.parse(endDate, formatterDateTime)
    countSundays(fromDate, toDate, 0)
  }

  @tailrec
  def countSundays(start: LocalDate, end: LocalDate, sundays: Int): Int = {
    if (start.isEqual(end.plusDays(1))) sundays
    else {
      if (start.getDayOfWeek.toString.equals("SUNDAY"))
        countSundays(start.plusDays(1), end, sundays + 1)
      else countSundays(start.plusDays(1), end, sundays)
    }
  }

  def check(e: String, regex: Regex): Boolean = e match {
    case null => false
    case e if e.trim.isEmpty => false
    case e if regex.findFirstMatchIn(e).isDefined => true
    case _ => false
  }

  def obfuscateEmail(text: String): String = {
    val emailRegex = """^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    if (check(text, emailRegex)) {
      val splittedText = text.toLowerCase.split('@')
      val namePart = splittedText(0)
      val domainPart = splittedText(1)
      namePart.charAt(0) + "*****" + namePart.charAt(namePart.length - 1) + "@" + domainPart
    } else throw new Exception("Not a valid email!")
  }

  def obfuscatePhone(text: String): String = {
    val phoneRegex = "^([+]|[0]{2})([0-9]|[ -])*$".r
    if (check(text, phoneRegex)) {
      text.replaceAll("\\d(?=(?:\\D*\\d){4})", "*").replaceAll(" ", "-")
    } else throw new Exception("Not a valid Phone number")
  }
}
