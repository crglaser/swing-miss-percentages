aggregate_batter_by_pitch_type <- function(df){
  agg <- aggregate(cbind(df$swing, df$miss, df$pitch, df$spin, df$start_speed, df$pfx_x, df$pfx_z), by=list(Category=df$batter_id, df$mlbam_pitch_name), FUN=sum)
  colnames(agg) <- c("batter_id", "mlbam_pitch_name", "pt_swings_bat", "pt_misses_bat", "pt_pitches_bat", "pt_total_spin_bat", "pt_total_start_speed_bat", "pt_total_pfx_x_bat", "pt_total_pfx_z_bat")
  
  agg$pt_swing_rate_bat <- agg$pt_swings_bat / agg$pt_pitches_bat
  agg$pt_miss_per_swing_bat <- ifelse(agg$pt_swings_bat > 0 , agg$pt_misses_bat / agg$pt_swings_bat, 0)
  agg$pt_spin_bat <- agg$pt_total_spin_bat / agg$pt_pitches_bat
  agg$pt_start_speed_bat <- agg$pt_total_start_speed_bat / agg$pt_pitches_bat
  agg$pt_pfx_x_bat <- agg$pt_total_pfx_x_bat / agg$pt_pitches_bat
  agg$pt_pfx_z_bat <- agg$pt_total_pfx_z_bat / agg$pt_pitches_bat
  
  return(agg)
}