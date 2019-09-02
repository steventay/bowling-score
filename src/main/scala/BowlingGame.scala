// Type alias to represent points and bowled result
type Points = Int
type Bowled = Int


/**
 * Special symbols that annotate the scores of a bowling game
 */
val Strike = 'X'
val Spare = '/'
val Miss = '-'
val Unknown = '_' // unknown score in the current frame

val Annotations = Map(
  Miss -> 0,
  Strike -> 10,
  Spare -> -1
)


/**
 * Contains a list of frame played. Initialises to an empty scorecard, i.e. no frames played.
 */
case class ScoreCard(frames: Seq[Frame] = Nil)

/**
 * A frame is a round of a bowling and constitutes 1/10th of a game.
 *
 * @param num    sequence number of the frame. From 1 to 10
 * @param result result of the frame from 1 to 2 throws
 * @param score  score of the current frame
 * @param total  running total of the game
 */
case class Frame(num: Int, result: Seq[Bowled], score: Points, total: Points)

/**
 * Basic model of a bowling game. Creates a new score card and scores the game.
 */
class BowlingGame {

  /**
   * Creates a new game and returns a new score card
   */
  def newGame(): ScoreCard = ScoreCard()

  /**
   * Scores the game with the throws for a single frame
   *
   * @param sc     score card to update the scores
   * @param t1     first throw of the frame
   * @param throws any subsequent throws of the frame
   * @return updated score card of the frame, including the running total of the game
   */
  def score(sc: ScoreCard, t1: Bowled, throws: Bowled*): ScoreCard = ???


  /**
   * Checks the bowled scores are valid
   *
   * @param t      pins bowled in a single throw
   * @param throws additional throws by the player in a frame
   */
  def validate(t: Bowled, throws: Bowled*): Unit = {
    require(t != Spare, "Invalid score. You cannot score a spare on the first throw!")
    val bowled = (t +: throws).map { t =>
      if (t == Spare) 10 - t else
        Annotations.getOrElse(t.toChar, t)
    }.sum
    require(bowled >= 0, "Invalid score. Minimum pins bowled is 0!")
    require(bowled <= 10, "Invalid score. Maximum pins bowled is 10")
  }

}