add_strike_zone_bools <- function(df){
  #Right now this is a right handed version with -1 and 1 as the boundries
  #Eventually add a column of boundries based on handedness and use that
  df$in_zone_horizontal <- ifelse(df$px >= -1 & df$px <= 1, 1, 0)
  
  df$in_zone_vertical <- ifelse(df$pz >= df$sz_bot & df$pz <= df$sz_top, 1, 0)
  df$in_zone <- ifelse(df$in_zone_horizontal & df$in_zone_vertical, 1, 0)
  
  return(df)
}