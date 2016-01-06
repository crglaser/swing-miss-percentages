transform_pitch_locations <- function(df){
  df$vertical_center <- mapply(vertical_center, df$sz_top, df$sz_bot)
  df$horizontal_plus <- ifelse(df$px >= 0, df$px, 0)
  df$horizontal_minus <- ifelse(df$px < 0, abs(df$px), 0)
  df$vertical_plus <- ifelse(df$pz >= df$vertical_center, df$pz - df$vertical_center, 0)
  df$vertical_minus <- ifelse(df$pz < df$vertical_center, df$vertical_center - df$pz, 0)
  return(df)
}