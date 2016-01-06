prepare_data <- function(df){
  df <- add_swings_and_misses(df)
  df <- add_count_dummy_variables(df)
  df <- transform_pitch_locations(df)
}