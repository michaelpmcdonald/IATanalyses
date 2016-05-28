


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