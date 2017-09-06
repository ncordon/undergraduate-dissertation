infoSmallDisjuncts <- function(dataset){
  # make an unpruned C4.5 tree out of dataset
  tree <- RWeka::J48(Class ~ ., dataset,
                     control = Weka_control(U = list(TRUE), 
                                            M = list(1)))
  
  positiveCoverage <- leavesCoverage(tree, classAttr = "positive")
  negativeCoverage <- leavesCoverage(tree, classAttr = "negative")
  coverages <- c(positiveCoverage, negativeCoverage)
  sensitivity <- computeSensitivity(tree, dataset)
  
  list(numSmallDisjuncts = length(which(coverages <= 3)),
       meanCoverage = mean(coverages),
       sensitivity = sensitivity)
}
