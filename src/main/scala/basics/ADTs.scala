object ADTs {

  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)

  sealed trait Suit
  object Suit {
    final case object Diamond extends Suit
    final case object Hearts extends Suit
    final case object Spades extends Suit
    final case object Clubs extends Suit
    def create(char: Char): Option[Suit] = {
      char.toLower match {
        case 'd' => Some(Diamond)
        case 'h' => Some(Hearts)
        case 's' => Some(Spades)
        case 'c' => Some(Clubs)
        case _ => None
      }
    }
  }


  case class Rank private(value: Int) extends AnyVal
  object Rank {
    val map: Map[Char, Int] = Map(
      'A' -> 14,
      'K' -> 13,
      'Q' -> 12,
      'J' -> 11,
      'T' -> 10,
      '9' -> 9,
      '8' -> 8,
      '7' -> 7,
      '6' -> 6,
      '5' -> 5,
      '4' -> 4,
      '3' -> 3,
      '2' -> 2
    )
    def create(value: Char): Option[Rank] = map.get(value.toUpper).map(Rank(_))
    def compare(a: Rank, b: Rank): Int = (a.value - b.value) match {
      case n if n < 0 => -1
      case n if n > 0 => 1
      case _ => 0
    }
  }

  sealed case class Card private(suit: Suit, rank: Rank)
  object Card {
    def create(string: String): Option[Card] = {
      val chars: List[Char] = string.split("").map(_.charAt(0)).toList
      chars match {
        case suitChar :: rankChar :: Nil => for {
          suit <- Suit.create(suitChar)
          rank <- Rank.create(rankChar)
        } yield (Card(suit, rank))
        case _ => None
      }
    }
  }

  sealed case class Hand private(cards: Set[Card])
  object Hand {
    def create(cards: Set[Card]): Option[Hand] = {
      if (cards.size == 2) Some(Hand(cards)) else None
    }
  }

  sealed case class Board private(cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] = {
      if (cards.size == 4) Some(Board(cards)) else None
    }
  }


  sealed trait Combination {
    def value: Int
    def ranks: Array[Rank]
  }

  object Combination {
    final case class RoyalFlush(ranks: Array[Rank] = Nil.toArray, value: Int = 10) extends Combination
    final case class StraightFlush(ranks: Array[Rank], value: Int = 9) extends Combination
    final case class FourOfAKind(ranks: Array[Rank], value: Int = 8) extends Combination
    final case class FullHouse(ranks: Array[Rank], value: Int = 7) extends Combination
    final case class Flush(ranks: Array[Rank], value: Int = 6) extends Combination
    final case class Straight(ranks: Array[Rank], value: Int = 5) extends Combination
    final case class ThreeOfAKind(ranks: Array[Rank], value: Int = 4) extends Combination
    final case class TwoPair(ranks: Array[Rank], value: Int = 3) extends Combination
    final case class Pair(ranks: Array[Rank], value: Int = 2) extends Combination
    final case class HighCard(ranks: Array[Rank], value: Int = 1) extends Combination

    def compare(a: Combination, b: Combination): Int = (a.value - b.value) match {
      case n if n < 0 => -1
      case n if n > 0 => 1
      case _ =>  a.ranks.zip(b.ranks).foldLeft(0)((prev, pair) => prev match {
        case 0 => Rank.compare(pair._1, pair._2)
        case n => n
      })
    }
  }

  sealed case class TestCase private(board: Board, hands: Set[Hand])
  sealed case class TestResult private(hands: Array[Set[Hand]])
}
