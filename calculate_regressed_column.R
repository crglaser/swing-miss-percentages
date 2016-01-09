calculate_regressed_column <- function(measurement_numerator, measurement_denominator, regression_average, regression_denominator){
  regressed_column <- (measurement_numerator + (regression_average * regression_denominator)) / (regression_denominator + measurement_denominator)
  return(regressed_column)
}