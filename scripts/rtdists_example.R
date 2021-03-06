library(rtdists)
set.seed(0)
## identical calls (but different random values)
rt1 <- rdiffusion(40, a=1, v=2, t0=0.5)
head(rt1)
rt2 <- rdiffusion(500, a=1, v=2, t0=0.5, z=0.5, d=0, sz=0, sv=0, st0=0)
head(rt2)
# get density for random RTs:
sum(log(ddiffusion(rt1$rt, rt1$response, a=1, v=2, t0=0.5)))  # boundary is factor
sum(log(ddiffusion(rt1$rt, as.numeric(rt1$response), a=1, v=2, t0=0.5))) # boundary is numeric
sum(log(ddiffusion(rt1$rt, as.character(rt1$response), a=1, v=2, t0=0.5))) # boundary is character

sum(log(ddiffusion(rt2$rt, rt2$response, a=1, v=2, t0=0.5)))

# can we recover the parameters?
ll_diffusion <- function(pars, rt, boundary)
{
  densities <- tryCatch(
    ddiffusion(rt, boundary,
               a=pars[1], v=pars[2], t0=pars[3],
               z=0.5, sz=pars[4],
               st0=pars[5], sv=pars[6]),
    error = function(e) 0)
  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}
## Not run:
start <- c(runif(2, 0.5, 3), 0.1, runif(3, 0, 0.5))
names(start) <- c("a", "v", "t0", "sz", "st0", "sv")
recov <- nlminb(start, ll_diffusion, lower = 0, rt=rt1$rt, boundary=rt1$response)
round(recov$par, 3)
#     a     v    t0    sz   st0    sv
# 1.017 2.186 0.505 0.345 0.000 0.000
## End(Not run)
# plot density:
curve(ddiffusion(x, a=1, v=2, t0=0.5, boundary = "upper"),
      xlim=c(0,3), main="Density of upper responses", ylab="density", xlab="response time")
curve(ddiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary = "upper"),
      add=TRUE, lty = 2)
legend("topright", legend=c("no", "yes"), title = "Starting Point Variability?", lty = 1:2)
# plot cdf:
curve(pdiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary="u"),
      xlim = c(0, 3),ylim = c(0,1),
      ylab = "cumulative probability", xlab = "response time",
      main = "CDF of diffusion model with start point variability")
curve(pdiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary="l"),
      add=TRUE, lty = 2)
legend("topleft", legend=c("upper", "lower"), title="boundary", lty=1:2)
### qLBA can only return values up to maximal predicted probability:
# maximum probability for a given set
pdiffusion(20, a=1, v=2, t0=0.5, st0=0.2, sz = 0.1, sv = 0.5, boundary="u")
# [1] 0.8705141
# equal but much slower:
# pdiffusion(Inf, a=1, v=2, t0=0.5, st0=0.2, sz = 0.1, sv = 0.5, boundary="u")
# [1] 0.8705141
qdiffusion(0.87, a=1, v=2, t0=0.5, st0=0.2, sz = 0.1, sv = 0.5, boundary="u")
# [1] 1.769253
