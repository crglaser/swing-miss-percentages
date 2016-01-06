add_swings_and_misses <- function(df){
  df$swing <- ifelse(df$pdes == "Ball" | df$pdes == "Ball In Dirt" | df$pdes == "Pitchout"
                      | df$pdes == "Hit By Pitch" | df$pdes == "Intent Ball" |
                       df$pdes == "Called Strike", 0, 1)
  df$miss <- ifelse(df$pdes == "Swinging Strike" | df$pdes == "Swinging Strike (Blocked)" 
                    | df$pdes == "Swinging Pitchout" | df$pdes == "Missed Bunt", 1, 0)
  return(df)
}