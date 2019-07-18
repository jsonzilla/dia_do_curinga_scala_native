package ddc

import ddc.FrodeCal.FixYear

trait CalendarCard {
  def card(d: Date): Either[String, Card]

  def show(d: Date): String = {
    card(d) match {
      case Right(a) => a.toString
      case Left(a) => "\nError in CalendarCard >> " + a
    }
  }
  def showComplete(d: Date): String = {
      card(d) match {
      case Right(a) => a.toStringLongFormat
      case Left(a) => "\nError in CalendarCard >> " + a
    }
  }
}

object YearCard extends CalendarCard {
  def card(d: Date) : Either[String, Card]  = {
    val ny = FixYear(d.year)
    def suit: Int = (ny / 13) % 4
    def cardNumber: Int = ny % 13
    Right(new Card(cardNumber, suit))
  }
}

object MonthCard extends CalendarCard {
  def card(d: Date) : Either[String, Card]  = {
    val nd = FrodeCal.ConvertDayCounter(d)
    nd match {
      case Left(a) => Left(a)
      case Right(ndd) =>
        def suit : Int = {
          val leap = Date.IsLeapYearInt(d.year - 1)
          d.day match {
            case day if day <= (62 - leap) => 1
            case day if day <= (154 - leap) => 2
            case day if day <= (247 - leap) => 3
            case day if day <= (338 - leap) => 0
            case day if day <= (367 - leap) => 1
            case _ => 1
          }
        }
        def cardNumber: Int = (ndd / 28) % 13
        Right(new Card(cardNumber, suit))
    }
  }
}

object WeekCard extends CalendarCard {
  def card(d: Date) : Either[String, Card] = {
    val nd = FrodeCal.ConvertDayCounter(d)
    nd match {
      case Left(a) => Left(a)
      case Right(ndd) =>
        def suit: Int = ((ndd / 7) / 13) % 4
        def cardNumber: Int = (ndd / 7) % 13
        Right(new Card(cardNumber, suit))
    }
  }
}

object DayCard extends CalendarCard {
  def card(d: Date) : Either[String, Card] = {
    val nd = FrodeCal.ConvertDayCounter(d)
    nd match {
      case Left(a) => Left(a)
      case Right(ndd) =>
        def suit: Int = {
          if (ndd == 0) 4
          else ((ndd - 1) / 13) % 4
        }
        def cardNumber: Int = {
          if (ndd == 0) 13
          else (ndd - 1) % 13
        }
        Right(new Card(cardNumber, suit))
    }
  }
}