// Type alias to represent points and bowled result
type Points = Int
type Bowled = Int

/**
 * Special symbols that annotate the scores of a bowling game
 */
val annotations = Map(
  '-' -> 0, // a miss
  'X' -> 10, // strike
  '/' -> -1, // spare
  '_' -> -999 // unknown score in the current frame
)


/**
 * Contains a list of frame played. Initialises to an empty scorecard, i.e. no frames played.
 */
case class ScoreCard(frames: List[Frame] = Nil)

/**
 * A frame is a round of a bowling and constitutes 1/10th of a game.
 *
 * @param num    sequence number of the frame. From 1 to 10
 * @param result result of the frame from 1 to 2 throws
 * @param score  score of the current frame
 * @param total  running total of the game
 */
case class Frame(num: Int, result: List[Bowled], score: Points, total: Points)

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

}