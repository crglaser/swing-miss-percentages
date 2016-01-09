compare_actual_predicted_miss_rates <- function(df){
  miss_rates <- aggregate(cbind(df$miss, df$predicted_miss, df$swing, df$predicted_swing_percentage, df$predicted_miss_per_swing), by=list(Category=df$pitcher_id), FUN=mean)
  colnames(miss_rates) <- c("pitcher_id", "actual_miss_rate", "predicted_miss_rate", "actual_swing_rate", "predicted_swing_rate", "predicted_miss_per_swing")
  pitch_totals <- test_2 <- aggregate(cbind(df$pitch, df$miss, df$swing), by=list(Category=df$pitcher_id), FUN=sum)
  colnames(pitch_totals) <- c("pitcher_id", "pitches_thrown", "misses", "swings")
  all <- join(miss_rates, pitch_totals, by = "pitcher_id")
  all$actual_miss_per_swing <- all$misses / all$swings
  all$diff_miss_rate <- all$predicted_miss_rate - all$actual_miss_rate
  all$diff_swing_rate <- all$predicted_swing_rate - all$actual_swing_rate
  all$diff_miss_per_swing <- all$predicted_miss_per_swing - all$actual_miss_per_swing
  return(all)
}