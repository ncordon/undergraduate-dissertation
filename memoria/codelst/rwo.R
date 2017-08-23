newSamples <- lapply(minority, function(x){
  # If attribute is numeric, generate new minority sample 
  # preserving mean and variance of existent samples
  if(is.numeric(x)){
    variance <- stats::var(x)
    x - variance/sqrt(n) * scaleFactors
    
    # Else if attribute is not numeric, make a roulette 
    # of possible values for the attribute and their frequency
  } else{
    dist <- table(x)
    distValues <- names(dist)
    distProbs <- unname(dist)
    sample(distValues, length(x) * iterPerInstance, 
           replace = T, prob = distProbs)
  }
})