aggregate_pitcher_swings_misses <- function(df){
  df$pitch <- rep(1,nrow(df))
  aggs <- aggregate(cbind(df$swing, df$miss, df$pitch), by=list(Category=df$pitcher_id), FUN=sum)
  colnames(aggs) <- c("pitcher_id", "pitcher_swings", "pitcher_misses", "pitcher_pitches")
  return(aggs)
}