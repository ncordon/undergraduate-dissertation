probDist <- .genDistribution(minority)

while(.naReplace(stats::sd(lastSlides), Inf) >= threshold){
  minority <- gibbsSampler(probDist, minority)
  prediction <- predict(model, minority)
  misclassified <- minority[prediction != minorityClass, , 
                            drop = FALSE]
  newSamples <- rbind.data.frame(newSamples, misclassified)
  train <- rbind.data.frame(train, misclassified)
  trainClass <- .appendfactor(trainClass, 
                              rep(minorityClass, 
                                  nrow(misclassified)))
  model <- trainWrapper(wrapper, train, trainClass)
  prediction <- predict(model, validation)
  
  # Measure of the quality of the newTrain
  qMeasure <- .sensitivity(prediction, validationClass)
  lastSlides <- c(qMeasure, lastSlides)
  lastSlides <- lastSlides[1:slideWin]
}