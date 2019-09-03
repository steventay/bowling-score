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
    assert(sc.frameScores == Seq(30, 20, 15, 5))
    assert(sc.runningTotal == Seq(30, 50, 65, 70))
    assert(sc.gameScore == 70)
    assert(sc.lastFrameNum == 4)
  }

  it should "score last frames correctly" in {
    val frames = (9 to 1 by -1).map { i => Frame(i, Seq('-', '-'), 0) }
    val sc = Game.score(ScoreCard(frames), 'X', 'X', 'X')
    assert(sc.frameScores == Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 30))
    assert(sc.runningTotal == Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 30))
    assert(sc.gameScore == 30)
  }

  it should "score a 200 point game correctly" in {
    val fn: (ScoreCard, Bowled, Bowled*) => ScoreCard = Game.score

    val sc = fn(fn(fn(fn(fn(fn(fn(fn(fn(fn(Game.newGame, 'X'), 9, '/'), 'X'), 9, '/'), 'X'), 9, '/'), 'X'), 9, '/'), 'X'), 9, '/', 'X')
    assert(sc.frameScores == Seq(20, 20, 20, 20, 20, 20, 20, 20, 20, 20))
    assert(sc.runningTotal == Seq(20, 40, 60, 80, 100, 120, 140, 160, 180, 200))
    assert(sc.gameScore == 200)
  }

  it should "score a perfect game correctly" in {
    val fn: (ScoreCard, Bowled, Bowled*) => ScoreCard = Game.score

    val sc = fn(fn(fn(fn(fn(fn(fn(fn(fn(fn(Game.newGame, 'X'), 'X'), 'X'), 'X'), 'X'), 'X'), 'X'), 'X'), 'X'), 'X', 'X', 'X')
    assert(sc.frameScores == Seq(30, 30, 30, 30, 30, 30, 30, 30, 30, 30))
    assert(sc.runningTotal == Seq(30, 60, 90, 120, 150, 180, 210, 240, 270, 300))
    assert(sc.gameScore == 300)
  }

  "The sample from liveabout.com" should "work" in {
    val fn: (ScoreCard, Bowled, Bowled*) => ScoreCard = Game.score
    val sc1 = fn(fn(fn(fn(fn(fn(fn(fn(fn(fn(Game.newGame, 'X'), 7, '/'), 7, 2), 9, '/'), 'X'), 'X'), 'X'), 2, 3), 6, '/'), 7, '/', 3)
    assert(sc1.frameScores == Seq(20, 17, 9, 20, 30, 22, 15, 5, 17, 13))
    assert(sc1.runningTotal == Seq(20, 37, 46, 66, 96, 118, 133, 138, 155, 168))
    assert(sc1.gameScore == 168)
  }
}
