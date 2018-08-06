# Title
# Author
# Description
# Date

assign_parameters <- function(){

  a <- 1
  b <- 2

  parameters <- as.list(sys.frame(sys.nframe()))
  return(parameters)

}

compute_inputs <- function(parameters){

  with(parameters, {
  
    c <- b + 1

    inputs <- as.list(sys.frame(sys.nframe()))
    return(inputs)

  })
}

simulate <- function(x, inputs){

  with(inputs, {
  
    y <- x + c * x + b * x * x + a * x * x * x

    results <- list(y = y)
    return (results)

  })

}

compute_outputs <- function(x, inputs, results){

  with(results, {
  
    z <- y + 1

    outputs <- list(x = x, y = y, z = z)
    return(outputs)
  })

}

# the executive function
run_simulation <- function(parameters){

  # get inputs based on latest parameter values
  inputs <- compute_inputs(parameters)

  # compute the independent variable
  x <- 1:3

  # run
  results <- simulate(x, inputs)
  
  # compute and format output for use by R-Vis
  outputs <- compute_outputs(x, inputs, results)
  return(outputs)
}

# the parameters list
# R-Vis will edit a copy of this object and 
# use the copy to invoke the executive 
parameters <- assign_parameters()

# R-Vis will invoke the executive like this
# outputs <- run_simulation(parameters)
