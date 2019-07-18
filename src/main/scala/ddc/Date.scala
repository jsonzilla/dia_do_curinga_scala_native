package ddc

import ddc.Date.{IsLeapYear}

class Date(y: Int, m: Int = 1, d: Int= 1) {
  val year: Int = y
  val month: Int = m
  val day: Int = d

  private def feb: Boolean = 
    day <= (28 + Date.IsLeapYearInt(year)) 

  def IsValidDay: Boolean =
    day > 0 && day < 32 && IsValidByMonth

  def IsValidByMonth: Boolean = month match {
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => day <= 31
    case 4 | 6 | 9 | 11 => day <= 30
    case 2 => feb
    case _ => false
  }

  def IsValidYear: Boolean = 
    year != 0

  def IsValid: Boolean = 
   IsValidDay && IsValidByMonth && IsValidYear
}

object Date {
  def IsLeapYear(year: Int) : Boolean = {
     (year % 400 == 0) || (year % 4 == 0 && year % 100 != 0)
  }

  def IsLeapYearInt(year: Int) : Int = {
    if (IsLeapYear(year)) 1 else 0
  }
}
