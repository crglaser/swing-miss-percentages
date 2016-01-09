predict_and_add_swing_miss_probabilities <- function(df, swing_regression, miss_regression){
  df$predicted_swing_percentage <- predict(swing_regression, df, type = "response")
  df$predicted_miss_per_swing <- predict(miss_regression, df, type = "response")
  df$predicted_miss <- df$predicted_swing_percentage * df$predicted_miss_per_swing
  return(df)
}