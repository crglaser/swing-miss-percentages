prepare_data <- function(df){
  #Add boolean columns to denote a swing and/or miss on each pitch
  df <- add_swings_and_misses(df)
  
  #Aggregate swings, misses and pitches by batter id and join to each pitch. 
  #Note: This should use previous year data once available.
  batter_agg <- aggregate_batter_swings_misses_pitches(df)
  df <- join(df, batter_agg, by = "batter_id")
  
  #Calculate swing percentage and miss per swing for the batter based on joined data
  df$batter_swing_percentage <- df$batter_swings / df$batter_pitches
  df$batter_miss_per_swing <- ifelse(df$batter_swings > 0, df$batter_misses / df$batter_swings, 0)
  
  #Aggregate swings, misses and pitches by pitcher id and join to each pitch. 
  #Note: This should use previous year data once available.
  pitcher_agg <- aggregate_pitcher_swings_misses(df)
  df <- join(df, pitcher_agg, by = "pitcher_id")
  
  #Calculate swing percentage and miss per swing for the pitcher based on joined data
  df$pitcher_swing_percentage <- df$pitcher_swings / df$pitcher_pitches
  df$pitcher_miss_per_swing <- ifelse(df$pitcher_swings > 0, df$pitcher_misses / df$pitcher_swings, 0)
  
  #Oops, this doesn't work - need to compare for the same id.
  #df$is_pitcher <- ifelse(df$pitcher_pitches >= df$batter_pitches, 1, 0)
  
  #calculate league swing percentage and miss per swing
  league_swing_percentage <- sum(df$swing) / sum(df$pitch)
  league_miss_per_swing <- sum(df$miss) / sum(df$swing)
  
  df$batter_pitcher_swing_percentage <- odds_ratio(df$batter_swing_percentage, df$pitcher_swing_percentage, league_swing_percentage)
  df$batter_pitcher_miss_per_swing <- odds_ratio(df$batter_miss_per_swing, df$pitcher_miss_per_swing, league_miss_per_swing)
  
  df <- add_count_dummy_variables(df)
  df <- transform_pitch_locations(df)
}