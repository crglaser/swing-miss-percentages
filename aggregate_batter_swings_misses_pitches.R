aggregate_batter_swings_misses_pitches <- function(df){
  df$pitch <- rep(1,nrow(df))
  aggs <- aggregate(cbind(df$swing, df$miss, df$pitch), by=list(Category=df$batter_id), FUN=sum)
  colnames(aggs) <- c("batter_id", "batter_swings", "batter_misses", "batter_pitches")
  return(aggs)
}