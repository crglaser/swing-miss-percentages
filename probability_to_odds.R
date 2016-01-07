probability_to_odds <- function(probability){
  odds = ifelse(probability == 1, 100, probability / (1-probability))
  return(odds)
}