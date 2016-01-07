prepare_data <- function(df){
  df <- add_swings_and_misses(df)
  
  batter_agg <- aggregate_batter_swings_misses_pitches(df)
  df <- join(df, batter_agg, by = "batter_id")
  
  df$batter_swing_percentage <- df$batter_swings / df$batter_pitches
  df$batter_miss_per_swing <- df$batter_misses / df$batter_swings
  
  pitcher_agg <- aggregate_pitcher_swings_misses(df)
  df <- join(df, pitcher_agg, by = "pitcher_id")
  
  df$pitcher_swing_percentage <- df$pitcher_swings / df$pitcer_pitches
  df$pitcher_miss_per_swing <- df$pitcher_misses / df$pitcher_swings
  
  df$is_pitcher <- ifelse(df$pitcher_pitches >= df$batter_pitches, 1, 0)
  
  df <- add_count_dummy_variables(df)
  df <- transform_pitch_locations(df)
}