swing_regression <- function(df){
  logit <- glm(swing ~ count01 + count02 + count10 + count11 + count12
               + count20 + count21 + count22 + count30 + count31 + count32 
               + vertical_plus + vertical_minus + horizontal_plus + horizontal_minus
               + spin + start_speed + pfx_x + pfx_z
               + batter_swing_percentage_regressed  
               + pitcher_swing_percentage_regressed
               + in_zone_horizontal + in_zone_vertical + in_zone
               + diff_start_speed_bat + diff_spin_bat 
               + diff_pfx_x_bat + diff_pfx_z_bat
               + pt_swing_rate_bat + pt_miss_per_swing_bat
               + diff_start_speed + diff_spin 
               + diff_pfx_x + diff_pfx_z
               + pt_swing_rate + pt_miss_per_swing,
               + diff_spin_fb + diff_pfx_x_fb + diff_pfx_z_fb + diff_start_speed_fb,
               #+ batter_pitcher_swing_percentage + batter_pitcher_miss_per_swing, 
               data = df, family = "binomial")
  return(logit)
}