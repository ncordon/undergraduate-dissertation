probDist <- .genDistribution(minority)

for(k in seq_len(iterations)){
  # Generate new sample using Gibbs Sampler
  minority <- gibbsSampler(probDist, minority)

  if(k > burnin && k%% lag == 0)
    newSamples <- rbind.data.frame(newSamples, minority)
}

