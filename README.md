# bowling-score
Works through how a bowling game is scored. 

## General Approach
1. Create suitable models / data structures
    - Core models include BowlingGame, ScoreCard and Frame     
2. Focused on the validation of the scores
    - Perhaps a bit too much?
3. Using the suggested function as a basis to work out a scoring algorithm for simplest scenario 
and iteratively work through the more challenging ones.
    - Open frames - Easiest
    - Spare - Needs to implement look ahead by 1
    - Strike - Needs to implement look ahead by 2
    - Detection and adjustment of the final frame w.r.t scoring
4. Each scenario was tested before proceeding to the next. Special tests include:
    - Perfect game ~ 300
    - Scoring 200 points
    - Example from bowling.about.com
       
## Scoring Algorithm
- Breaks into 2 parts: current frame and previous frames
- Tries to score current frame if it is an open frame otherwise it will wait for future scores.
- For every new frame we walk through all previous frames and iteratively "re-score" the strikes and spares.
- Need to watch out for special conditions for consecutive strikes where it needs to look ahead to 2 throws for scoring.  
- 10th frame is relatively easy once the above is done. For the 10th frame we convert the scores to 10 if it was either a spare or strike. 
- Need to consider additional fill-balls: 2 for strikes and 1 for spares in the 10th frame.
