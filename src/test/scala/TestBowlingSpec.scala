import BowlingGame.Bowled
import org.scalatest.FlatSpec

class TestBowlingSpec extends FlatSpec {

  val Game = BowlingGame

  "A new bowling game" should "have an empty score card" in {
    assert(BowlingGame.newGame.frames.isEmpty)
  }

  "A bowling game" should "pass valid scores" in {
    Game.validate('-', '-')
    Game.validate('X')
    Game.validate(4, 5)
    Game.validate('-', '/')
  }

  it should "fail more than 2 throws" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(1, 2, 3)
    }
  }

  it should "fail when a spare is thrown in the first throw" in {
    assertThrows[IllegalArgumentException] {
      Game.validate('/')
    }
  }

  it should "fail when a strike is recorded in the second throw" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(5, 'X')
    }
  }

  it should "fail when a strike is recorded but a second throw is scored" in {
    assertThrows[IllegalArgumentException] {
      Game.validate('X', '-')
    }
  }

  it should "fail when pins bowled is less than 0" in {
    assertThrows[IllegalArgumentException] {
      Game.validate('X', -10)
    }
  }

  it should "fail when pins bowled is more than 10" in {
    assertThrows[IllegalArgumentException] {
      Game.validate('-', 20)
    }
  }

  it should "fail when total is more than 10" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(9, 8)
    }
  }

  "The score card" should "score open frames correctly" in {
    val fn: (ScoreCard, Bowled, Bowled *) => ScoreCard = Game.score
    val sc = fn(fn(fn(fn(Game.newGame, 1, 2), 3, 4), 4, 5), '-', 5)
    assert(sc.runningTotal == Seq(3, 10, 19, 24))
    assert(sc.gameScore == 24)
  }
}
