prepare_data <- function(df){
  #Make sure to load the necessary packages
  load_packages()
  
  #Add boolean columns to denote a swing and/or miss on each pitch
  df <- add_swings_and_misses(df)
  df <- add_in_strike_zone_bools(df)
  
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
  
  #set regression amounts - change this to take in arguments w/ default values.
  #separate by batter/pitcher?
  swing_regression_pitches = 10
  miss_regression_swings = 10
  
  #calculate regressed rates for batters
  df$batter_swing_percentage_regressed <- calculate_regressed_column(df$batter_swings, df$batter_pitches, league_swing_percentage, swing_regression_pitches)
  df$batter_miss_per_swing_regressed <- calculate_regressed_column(df$batter_misses, df$batter_swings, league_miss_per_swing, miss_regression_pitches)
  
  #calculate regressed rates for pitchers
  df$pitcher_swing_percentage_regressed <- calculate_regressed_column(df$pitcher_swings, df$pitcher_pitches, league_swing_percentage, swing_regression_pitches)
  df$pitcher_miss_per_swing_regressed <- calculate_regressed_column(df$pitcher_misses, df$pitcher_swings, league_miss_per_swing, miss_regression_pitches)
  
  #calculate odds ratio matchup rates - maybe use regressed here or calculate both?
  df$batter_pitcher_swing_percentage <- odds_ratio(df$batter_swing_percentage, df$pitcher_swing_percentage, league_swing_percentage)
  df$batter_pitcher_miss_per_swing <- odds_ratio(df$batter_miss_per_swing, df$pitcher_miss_per_swing, league_miss_per_swing)
  
  df <- add_count_dummy_variables(df)
  df <- transform_pitch_locations(df)
}