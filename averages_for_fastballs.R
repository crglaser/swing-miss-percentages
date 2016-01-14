averages_for_fastballs <- function(df){
  fb <- subset(df, mlbam_pitch_name == "FA" | mlbam_pitch_name == "FF" | mlbam_pitch_name == "FT" || mlbam_pitch_name == "SI")
  fb_aggs <- aggregate(cbind(fb$start_speed, fb$pfx_x, fb$pfx_z, fb$spin), by=list(Category=fb$pitcher_id), FUN=mean)
  colnames(fb_aggs) <- c("pitcher_id", "fb_start_speed", "fb_pfx_x", "fb_pfx_z", "fb_spin")
  return(fb_aggs)
}