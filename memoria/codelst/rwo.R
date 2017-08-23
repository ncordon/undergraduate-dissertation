newSamples <- lapply(minority, function(x){
  # If attribute is continuous, generate new minority
  # sample preserving mean and variance of existent samples
  scaleFactors <- stats::rnorm(nrow(minority) * iterations,
                               mean = 0, sd = 1)

  if(class(x) == "numeric"){
    variance <- (m-1)/m * stats::var(x)
    x - variance/sqrt(m) * scaleFactors

    # Else if attribute is not numeric, make a roulette out
    # of possible values for the attribute and their frequency
  } else{
    dist <- table(x)
    distValues <- names(dist)
    distProbs <- unname(dist)
    sample(distValues, length(x) * iterations, replace = T, prob = distProbs)
  }
})
