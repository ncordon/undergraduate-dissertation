######################################################################
# Extract coverage for each leave of the tree
######################################################################
leavesCoverage <- function(tree, classAttr){
  # retrieve tree in string format
  strgraph <- tree$classifier$graph()
  
  # select leaves corresponding to specified classAttr
  listmatches <- unlist(strsplit(strgraph,
                                 paste(classAttr, "\\(", sep = " ")))
  whichNumeric <- grep("^[[:digit:]]+", listmatches, perl = TRUE)
  listmatches <- listmatches[whichNumeric]
  
  # extract coverage number
  coverages <- lapply(listmatches, function(x){
    y <- sub(").*", "", x)
    sub("/.*", "", y)
  })
  coverages <- as.numeric(unlist(coverages))
  coverages[coverages != 0]
}

######################################################################
# Divide data in 3 partitions
######################################################################
makePartition <- function(dataset, numPartitions){
  folds <- createFolds(dataset$Class, numPartitions)
  
  # Returns map of folds to the original data
  lapply(folds, function(fold) dataset[fold, ])
}


dataFolds <- lapply(datasets, function(d){
  makePartition(eval(as.name(d)), numPartitions)
})
names(dataFolds) <- datasets

######################################################################
# We consider a small disjunct a rule covering less or equal
# than 3 samples. This function outputs number of small disjuncts
# and mean of rule coverages
######################################################################
infoSmallDisjuncts <- function(dataset){
  # make an unpruned C4.5 tree out of dataset
  tree <- RWeka::J48(Class ~ ., dataset,
                     control = Weka_control(U = list(TRUE), M = list(1)))
  
  positiveCoverage <- leavesCoverage(tree, classAttr = "positive")
  negativeCoverage <- leavesCoverage(tree, classAttr = "negative")
  coverages <- c(positiveCoverage, negativeCoverage)
  
  list(numSmallDisjuncts = length(which(coverages <= 3)),
       meanCoverage = mean(coverages))
}