import BowlingGame._

import scala.annotation.tailrec

/**
 * Contains a list of frame played.
 * Initialises to an empty scorecard, i.e. no frames played and scores for each frame entered.
 */
case class ScoreCard(frames: Seq[Frame] = Nil) {

  /**
   * Scores the current frame and recalculates previous frames' scores if necessary
   *
   * @param throws rolls for the current frame
   * @return updated scorecard with the current frame
   */
  def scoreFrame(throws: Seq[Bowled]): ScoreCard = {
    val curFrame = scoreCur(throws)
    val prev = rescorePrev(throws)
    ScoreCard(curFrame +: prev)
  }

  /**
   * Calculates the current frame score if possible
   *
   * @param throws rolls for the current frame
   * @return frame with the current score
   */
  private def scoreCur(throws: Seq[Bowled]): Frame = {
    val num = frames.headOption.map(_.num + 1).getOrElse(1)

    if (lastFrameNum + 1 != 10 && (throws.contains(Strike) || throws.contains(Spare) || throws.sum == 10))
      Frame(num, throws, Unscored)
    else {
      Frame(num, throws, convertPoints(throws).sum)
    }
  }

  /**
   * Re-scores the previous frames in relation to the current thrown balls
   *
   * @param throws rolls for the current frame
   * @return previous frames with scores calculated
   */
  private def rescorePrev(throws: Seq[Bowled]): Seq[Frame] = {
    frames.foldLeft(Seq.empty[Frame]) { (scored, f) =>
      val t = convertPoints(resultsAfter(f.num) ++ throws)
      val sf = if (f.isScored) f else {
        (f.isStrike, f.isSpare) match {
          case (true, _) if t.size >= 2 => Frame(f.num, f.result, 10 + t.take(2).sum)
          case (_, true) => Frame(f.num, f.result, 10 + t.head)
          case _ => f
        }
      }
      scored :+ sf
    }
  }

  /**
   * Gets the scores for all the frames
   */
  def frameScores: Seq[Points] = frames.map(_.score).reverse

  /**
   * Gets the latest game score
   */
  def gameScore: Points = if (frames.exists(!_.isScored)) Unscored else frames.map(_.score).sum

  /**
   * Gets the cumulative scores across all the frames scored
   */
  def runningTotal: Seq[Points] = {
    frames.foldRight(Seq.empty[Points]) { (f, total) =>
      (f.score + total.headOption.getOrElse(0)) +: total
    }.reverse
  }

  /**
   * Gets the scored results after the last frame
   */
  def resultsAfter(frameNum:Int): Seq[Bowled] = frames.filter(_.num > frameNum).flatMap(_.result)

  def lastFrameNum: Int = frames.headOption.map(_.num).getOrElse(0)

}

/**
 * A frame is a round of a bowling and constitutes 1/10th of a game.
 *
 * @param num    sequence number of the frame. From 1 to 10
 * @param result result of the frame from 1 to 2 throws
 * @param score  score of the current frame
 */
case class Frame(num: Int, result: Seq[Bowled], score: Points) {
  def isSpare: Boolean = result.contains(Spare)

  def isStrike: Boolean = result.contains(Strike)

  def isScored: Boolean = score != Unscored
}

/**
 * Basic model of a bowling game. Creates a new score card and scores the game.
 */
object BowlingGame {
  // Type alias to represent points and bowled result
  type Points = Int
  type Bowled = Int

  /**
   * Special symbols that denote special scores of a bowling game
   */
  val Strike = 'X'
  val Spare = '/'
  val Miss = '-'
  val Unscored = '_' //  no score in the current frame

  private val ValidPoints = 0 to 10


  /**
   * Creates a new game and returns a new score card
   */
  def newGame: ScoreCard = ScoreCard()

  /**
   * Scores the game with the throws for a single frame
   *
   * @param sc     score card to update the scores
   * @param t1     first throw of the frame
   * @param throws any subsequent throws of the frame
   * @return updated score card of the frame, including the running total of the game
   */
  def score(sc: ScoreCard, t1: Bowled, throws: Bowled*): ScoreCard = {
    validate(sc, t1, throws: _*)
    sc.scoreFrame(t1 +: throws)
  }

  /**
   * Checks the bowled scores are valid
   *
   * @param t      pins bowled in a single throw
   * @param throws additional throws by the player in a frame
   */
  def validate(sc: ScoreCard, t: Bowled, throws: Bowled*): Unit = {
    assert(sc.lastFrameNum + 1 <= 10, "Game has already ended!")

    if (sc.lastFrameNum + 1 != 10) {
      require(throws.size <= 1, "Only 2 throws allowed in a non-final frame!")
      if (t == Strike)
        require(throws.isEmpty, "Invalid score. No second throw is needed!")
      require(!throws.contains(Strike), "Invalid score. You cannot score a strike on the second throw!")
    }

    require(t != Spare, s"Invalid score. You cannot score a spare on the first throw!")


    // Final frame validation
    if (sc.lastFrameNum + 1 == 10) {
      if (t == Strike || throws.head == Spare)
        require(throws.size == 2, s"${2 - throws.size} more throws required in the final frame!")
      else
        require(throws.size == 1, "Fill balls are only allowed for strikes and spares!")
    }

    val bowled = convertPoints(t +: throws)

    if (sc.lastFrameNum + 1 != 10) {
      require(bowled.sum <= 10, "Invalid score. Maximum bowl score is 10!")
      require(bowled.forall(ValidPoints.contains), "Invalid score. Minimum score is 0 and maximum score is 10")
    }
  }

  /**
   * Resolves annotated scores to pure points. Assumes valid scores!
   *
   * @param throws bowled scores
   * @return bowled points
   */
  def convertPoints(throws: Seq[Bowled]): Seq[Points] = {
    @tailrec
    def convert(t: Seq[Bowled], acc: Seq[Points]): Seq[Points] = {
      if (t.isEmpty) acc
      else {
        val points = t.head match {
          case Strike => 10
          case Miss => 0
          case Spare => 10 - acc.last
          case p => p
        }
        convert(t.tail, acc :+ points)
      }
    }

    convert(throws, Seq.empty)
  }
}

