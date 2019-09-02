import org.scalatest.FlatSpec

class TestBowlingSpec extends FlatSpec {
  val game = new BowlingGame

  "A new bowling game" should "have an empty score card" in {
    assert(game.newGame().frames.isEmpty)
  }

  "A bowling game" should "pass valid scores" in {
    game.validate('-', '-')
    game.validate('X')
    game.validate(4, 5)
    game.validate('-', '/')
  }

  it should "fail more than 2 throws" in {
    assertThrows[IllegalArgumentException] {
      game.validate(1, 2, 3)
    }
  }

  it should "fail when a spare is thrown in the first throw" in {
    assertThrows[IllegalArgumentException] {
      game.validate('/')
    }
  }

  it should "fail when a strike is recorded in the second throw" in {
    assertThrows[IllegalArgumentException] {
      game.validate(5, 'X')
    }
  }

  it should "fail when pins bowled is less than 0" in {
    assertThrows[IllegalArgumentException] {
      game.validate('X', -10)
    }
  }

  it should "fail when pins bowled is more than 10" in {
    assertThrows[IllegalArgumentException] {
      game.validate('-', 20)
    }
  }

  it should "fail when total is more than 10" in {
    assertThrows[IllegalArgumentException] {
      game.validate(9, 8)
    }
  }
}
