aggregate_pitcher_by_pitch_type <- function(df){
  agg <- aggregate(cbind(df$swing, df$miss, df$pitch, df$spin, df$start_speed, df$pfx_x, df$pfx_z), by=list(Category=df$pitcher_id, df$mlbam_pitch_name), FUN=sum)
  colnames(agg) <- c("pitcher_id", "mlbam_pitch_name", "pt_swings", "pt_misses", "pt_pitches", "pt_total_spin", "pt_total_start_speed", "pt_total_pfx_x", "pt_total_pfx_z")
  
  agg$pt_swing_rate <- agg$pt_swings / agg$pt_pitches
  agg$pt_miss_per_swing <- agg$pt_misses / agg$pt_swings
  agg$pt_spin <- agg$pt_total_spin / agg$pt_pitches
  agg$pt_start_speed <- agg$pt_total_start_speed / agg$pt_pitches
  agg$pt_pfx_x <- agg$pt_total_pfx_x / agg$pt_pitches
  agg$pt_pfx_z <- agg$pt_total_pfx_z / agg$pt_pitches
  
  return(agg)
}