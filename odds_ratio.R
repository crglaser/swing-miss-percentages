odds_ratio <- function(hitter_probability, pitcher_probability, league_probability){
  hitter_odds <- probability_to_odds(hitter_probability)
  pitcher_odds <- probability_to_odds(pitcher_probability)
  league_odds <- probability_to_odds(league_probability)
  ratio <- (hitter_odds * pitcher_odds) / league_odds
  percentage <- ratio / (ratio + 1)
  return(percentage)
}