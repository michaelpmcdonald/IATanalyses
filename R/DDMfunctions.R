DDMestimate <- function(df){
  # print(head(df))
  class(df) <- 'data.frame'
  df <- df %>% select(q, resp)
  print(head(df))
  # Need to adjust the error trials since they include an extra button press
  # adjustment is mean(incorrect)-mean(correct)
  difference <- mean(df[df$resp=="lower",]$q) - mean(df[df$resp=="upper",]$q)
  if(!is.nan(difference)){
    df[df$resp=="lower",]$q <- df[df$resp=="lower",]$q-difference
  }
  if(wiener_deviance(x=c(1, .1, .1, 1), dat=df)==Inf) {
    message("Wiener deviance for subject ", subject,", pairing ", j, " could not be evaluated.")
    return(rep(NA, 4))
  }
  fit <- optim(c(1, .001, .001, 1), wiener_deviance, dat=df, method="Nelder-Mead")
  results <- paste(fit$par[1:4], letters[1:4], sep="_")
  return(data.frame(results))
}


#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

get.vaTer <- function(ID, Pc, VRT, MRT, s=0.1) {
  s2 = s^2
  # The default value for the scaling parameter s equals 0.1
  #   if (Pc == 0)
  #     cat("Oops, Pc == 0!\n")
  #   if (Pc == 0.5)
  #     cat("Oops, Pc == .5!\n")
  #   if (Pc == 1)
  #     cat("Oops, Pc == 1!\n")
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge correction is required.

  L = qlogis(Pc)
  # The function "qlogis" calculates the logit.

  x = L*(L*Pc^2 - L*Pc + Pc - .5)/VRT
  v = sign(Pc-.5)*s*x^(1/4)
  # This gives drift rate.

  a = s2*qlogis(Pc)/v
  # This gives boundary separation.

  y = -v * a / s2
  MDT = (a/(2*v)) * (1-exp(y))/(1+exp(y))
  Ter = MRT-MDT
  # This gives nondecision time.

  return(data.frame(ID, v, a, Ter))
}
