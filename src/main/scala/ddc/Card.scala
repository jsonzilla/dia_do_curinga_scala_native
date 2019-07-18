package ddc

class CardNumber(i: Int) {
  private def numerals(i: Int): Option[String] = {
    if (i < 1 || i > 13) None else Some(i.toString)
  }

  def shortFormat: Option[String] = i match {
    case 10 => Some("J")
    case 11 => Some("Q")
    case 12 => Some("K")
    case 13 => Some("Jo")
    case _ => numerals(i + 1)
  }

  def longFormat: Option[String] = i match {
    case 0 => Some("de As")
    case 1 => Some("de Dois")
    case 2 => Some("de Tres")
    case 3 => Some("de Quatro")
    case 4 => Some("de Cinco")
    case 5 => Some("de Seis")
    case 6 => Some("de Sete")
    case 7 => Some("de Sete")
    case 8 => Some("de Nove")
    case 9 => Some("de Dez")
    case 10 => Some("de Valete")
    case 11 => Some("de Dama")
    case 12 => Some("de Rei")
    case 13 => Some("do Curinga")
    case _ => None
  }
}

class Suit(i: Int) {
  def shortFormat: Option[String] = i match {
    case 0 => Some("O")
    case 1 => Some("P")
    case 2 => Some("C")
    case 3 => Some("E")
    case 4 => Some("")
    case _ => None
  }

  def longFormat: Option[String] = i match {
    case 0 => Some(" de ouros")
    case 1 => Some(" de paus")
    case 2 => Some(" de copas")
    case 3 => Some(" de espadas")
    case 4 => Some("")
    case _ => None
  }
}

class Card(cn: Int, s: Int) {
  val cardNumber = new CardNumber(cn)
  val suit = new Suit(s)

  private def CardPrint(p: (Option[String], Option[String])): String = p match {
    case (Some(a), Some(b)) => a + b
    case (Some(a), None) => "Error in card suit >> "
    case (None, Some(a)) => "Error in card number >> "
    case _ => "Error in card >> "
  }

  override def toString: String =
    CardPrint(cardNumber.shortFormat, suit.shortFormat)

  def toStringLongFormat: String =
    CardPrint(cardNumber.longFormat, suit.longFormat)
}
