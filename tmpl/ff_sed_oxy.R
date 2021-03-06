Flux <- matrix(ncol = 2, byrow = TRUE, data = c(
  1, 0.654, 11, 0.167, 21, 0.060, 41, 0.070, 73, 0.277, 83, 0.186,
  93, 0.140, 103, 0.255, 113, 0.231, 123, 0.309, 133, 1.127, 143, 1.923,
  153, 1.091, 163, 1.001, 173, 1.691, 183, 1.404, 194, 1.226, 204, 0.767,
  214, 0.893, 224, 0.737, 234, 0.772, 244, 0.726, 254, 0.624, 264, 0.439,
  274, 0.168, 284, 0.280, 294, 0.202, 304, 0.193, 315, 0.286, 325, 0.599,
  335, 1.889, 345, 0.996, 355, 0.681, 365, 1.135))

# the forcing functions; rule = 2 avoids NaNs in interpolation
Depo <- approxfun(x = Flux[, 1], y = Flux[, 2], method = "linear", rule = 2)

## the model
sediment <- function(t, O2, k) list(c(Depo(t) - k * O2), depo = Depo(t))

k <- 0.01

parms <- c(k = k)

times <- 1:365
out <- deSolve::ode(times = times, func = sediment, y = c(O2 = 63), parms = parms)
