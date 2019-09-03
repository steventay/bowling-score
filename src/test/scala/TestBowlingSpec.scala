import BowlingGame.Bowled
import org.scalatest.FlatSpec

class TestBowlingSpec extends FlatSpec {

  val Game: BowlingGame.type = BowlingGame

  "A new bowling game" should "have an empty score card" in {
    assert(BowlingGame.newGame.frames.isEmpty)
  }

  val sc: ScoreCard = Game.newGame

  "A bowling game" should "pass valid scores" in {
    Game.validate(sc, '-', '-')
    Game.validate(sc, 'X')
    Game.validate(sc, 4, 5)
    Game.validate(sc, '-', '/')
  }

  it should "pass valid scores in a final frame" in {
    val frames = (9 to 1 by -1).map { i => Frame(i, Seq('-', 5), 5) }
    val sc = ScoreCard(frames)
    Game.validate(sc, 'X', 5, 3)
    Game.validate(sc, '5', '/', 3)
  }

  it should "fail more than 2 throws in the non-final frame" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, 1, 2, 3)
    }
  }

  it should "fail more than 2 throws in the final frame if it isn't a strike or spare" in {
    assertThrows[IllegalArgumentException] {
      val frames = (9 to 1 by -1).map { i => Frame(i, Seq('-', 5), 5) }
      val sc = ScoreCard(frames)
      Game.validate(sc, 1, 2, 3)
    }
  }

  it should "fail when a spare is thrown in the first throw" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, '/')
    }
  }

  it should "fail when a strike is recorded in the second throw" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, 5, 'X')
    }
  }

  it should "fail when a strike is recorded but a second throw is scored" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, 'X', '-')
    }
  }

  it should "fail when pins bowled is less than 0" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, 'X', -10)
    }
  }

  it should "fail when pins bowled is more than 10" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, '-', 20)
    }
  }

  it should "fail when total is more than 10" in {
    assertThrows[IllegalArgumentException] {
      Game.validate(sc, 9, 8)
    }
  }

  "The game" should "convert notations correctly" in {
    assert(Game.convertPoints(Seq(1, '/')) == Seq(1, 9))
    assert(Game.convertPoints(Seq('-', 5)) == Seq(0, 5))
    assert(Game.convertPoints(Seq('X')) == Seq(10))
  }

  "The score card" should "score open frames correctly" in {
    val fn: (ScoreCard, Bowled, Seq[Bowled]) => ScoreCard = Game.score
    val sc = fn(fn(fn(fn(Game.newGame, 1, Seq(2)), 3, Seq(4)), 4, Seq(5)), '-', Seq(5))
    assert(sc.runningTotal == Seq(3, 10, 19, 24))
    assert(sc.gameScore == 24)
  }

  it should "score spare frames correctly" in {
    val fn: (ScoreCard, Bowled, Seq[Bowled]) => ScoreCard = Game.score
    val sc = fn(fn(fn(fn(Game.newGame, 1, Seq('/')), 2, Seq('/')), 3, Seq('/')), '-', Seq(5))
    assert(sc.frameScores == Seq(12, 13, 10, 5))
    assert(sc.runningTotal == Seq(12, 25, 35, 40))
    assert(sc.gameScore == 40)
  }

  it should "score strike frames correctly" in {
    val fn: (ScoreCard, Bowled, Bowled*) => ScoreCard = Game.score
    val sc = fn(fn(fn(fn(Game.newGame, 'X'), 'X'), 'X'), '-', 5)
    assert(sc.frameScores == Seq(30, 30, 15, 5))
    assert(sc.runningTotal == Seq(30, 60, 75, 80))
    assert(sc.gameScore == 80)
    assert(sc.lastFrameNum == 4)
  }
}
