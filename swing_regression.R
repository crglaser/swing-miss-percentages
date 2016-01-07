swing_regression <- function(df){
  logit <- glm(swing ~ count00 + count01 + count02 + count10 + count11 + count12
               + count20 + count21 + count22 + count30 + count31 + count32 
               + vertical_plus + vertical_minus + horizontal_plus + horizontal_minus
               + spin + start_speed + pfx_x + pfx_z, 
               data = df, family = "binomial")
  return(logit)
}