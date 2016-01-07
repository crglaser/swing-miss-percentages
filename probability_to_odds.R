probability_to_odds <- function(probability){
  odds = probability / (1-probability)
  return(odds)
}