import BowlingGame._

/**
 * Contains a list of frame played. Initialises to an empty scorecard, i.e. no frames played and scores for each frame
 * entered.
 */
case class ScoreCard(frames: Seq[Frame] = Nil) {

  /**
   * Scores the current frame and recalculates previous frames' scores if necessary
   *
   * @param throws rolls for the current frame
   * @return updated scorecard with the current frame
   */
  def scoreFrame(throws: Seq[Bowled]): ScoreCard = {
    val prev = rescore(throws)
    val curFrame = score(throws, prev)
    ScoreCard(prev :+ curFrame)
  }

  /**
   * Calculates the current frame score if possible
   *
   * @param throws rolls for the current frame
   * @param prev   previous frame with scores calculated
   * @return frame with the current score
   */
  private def score(throws: Seq[Bowled], prev: Seq[Frame]): Frame = {
    val num = prev.lastOption.map(_.num + 1).getOrElse(1)

    if (throws.contains(Strike) || throws.contains(Spare))
      Frame(num, throws, Unknown, Unknown)
    else {
      val total = prev.lastOption.map(_.total).getOrElse(0)
      val points = convertPoints(throws)
      Frame(num, throws, points.sum, total + points.sum)
    }

  }

  /**
   * Rescores the previous frames in relation to the current thrown balls
   *
   * @param throws rolls for the current frame
   * @return previous frames with scores calculated
   */
  private def rescore(throws: Seq[Bowled]): Seq[Frame] = {
    this.frames // Simply return the last frames for now
  }

  /**
   * Gets the latest game score
   */
  def gameScore: Points = frames.map(_.score).sum

  /**
   * Gets the cumulative scores across all the frames scored
   */
  def runningTotal: Seq[Points] = frames.map(_.total)

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
  val Unknown = '_' // unknown score in the current frame

  private val Notations = Map(
    Miss -> 0,
    Strike -> 10,
    Spare -> -1
  )

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
    validate(t1, throws: _*)
    sc.scoreFrame(t1 +: throws)
  }

  /**
   * Checks the bowled scores are valid
   *
   * @param t      pins bowled in a single throw
   * @param throws additional throws by the player in a frame
   */
  def validate(t: Bowled, throws: Bowled*): Unit = {
    require(throws.size <= 1, "Only 2 throws allowed in a frame!") // Doesn't consider 10th frame yet.
    require(t != Spare, s"Invalid score. You cannot score a spare on the first throw!")
    require(!throws.contains(Strike), "Invalid score. You cannot score a strike on the second throw!")

    if (t == Strike)
      require(throws.isEmpty, "Invalid score. No second throw is needed!")

    val bowled = convertPoints(t +: throws)

    require(bowled.forall(ValidPoints.contains), "Invalid score. Minimum score is 0 and maximum score is 10")
    require(bowled.sum <= 10, "Invalid score. Maximum bowl score is 10!")
  }

  /**
   * Resolves annotated scores to pure points. Assumes valid scores!
   *
   * @param throws bowled scores
   * @return bowled points for the first and subsequent throws
   */
  def convertPoints(throws: Seq[Bowled]): Seq[Points] = {
    val b1 = Notations.getOrElse(throws.head.toChar, throws.head)
    val bowled = (b1 +: throws.tail).map { b =>
      if (b == Spare) 10 - b1 else
        Notations.getOrElse(b.toChar, b)
    }
    bowled
  }
}

/**
 * A frame is a round of a bowling and constitutes 1/10th of a game.
 *
 * @param num    sequence number of the frame. From 1 to 10
 * @param result result of the frame from 1 to 2 throws
 * @param score  score of the current frame
 * @param total  running total of the game
 */
case class Frame(num: Int, result: Seq[Bowled], score: Points, total: Points)

