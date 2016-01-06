transform_pitch_locations <- function(df){
  df$horizontal_plus <- ifelse(df$px >= 0, df$px, 0)
  df$horizontal_minus <- ifelse(df$px < 0, abs(df$px), 0)
  df$vertical_plus <- ifelse(df$pz >= vertical_center(df$sz_top, df$sz_bot), df$pz - vertical_center(df$sz_top, df$sz_bot), 0)
  df$vertical_minus <- ifelse(df$pz < vertical_center(df$sz_top, df$sz_bot), vertical_center(df$sz_top, df$sz_bot) - df$pz, 0)
  return(df)
}