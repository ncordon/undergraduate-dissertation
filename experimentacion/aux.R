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
makePartition <- function(dataset, numPartitions, seed = 12345){
  set.seed(seed)
  folds <- createFolds(dataset$Class, numPartitions)

  # Returns map of folds to the original data
  lapply(folds, function(fold) dataset[fold, ])
}

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

######################################################################
# Function to get results. If filtering is set to TRUE, neater is used
# to clean new generated instances.
######################################################################
getResults <- function(filtering, seed = 12345){
  set.seed(seed)
  # Prepare matrix for results
  results <- matrix(ncol = length(datasets), nrow = length(algorithms))
  colnames(results) <- datasets
  rownames(results) <- algorithms
  resultsSizes <- results
  resultsNums <- results

  # For each algorithm, for each dataset, get numPartitions executions and calc
  # the mean size of coverages and the mean number of small disjuncts
  for(algName in algorithms){
    if(algName != "none")
      algorithm <- eval(as.name(algName))

    for(dataName in datasets){
      if(algName != "wracog" || (algName == "wracog" && dataName == "wisconsin")){
        overbalanced <- tryCatch(
          sapply(seq_along(dataFolds[[dataName]]), function(j){
            fold <- dataFolds[[dataName]][[j]]
            validation <- dataFolds[[dataName]][[j %% numPartitions + 1]]
            sizeMinority <- length(which(fold$Class == "positive"))
            sizeDataset <- nrow(fold)
            # We configure an imbalance ratio of 0.8
            numInstances <- ceiling(0.8 * (sizeDataset - sizeMinority) - sizeMinority)
            newSamples <- NULL
  
            if(algName != "none"){
              if (algName != "wracog"){
                newSamples <- algorithm(fold, numInstances)
              } else{
                newSamples <- algorithm(fold, validation, wrapper = J48Wrapper)
              }
  
              if (filtering && nrow(newSamples) > 0){
                newSamples <- neater(fold, newSamples, iterations = 200)
              }
  
              balancedFold <- rbind(fold, newSamples)
  
            } else{
              balancedFold <- fold
            }
            infoSmallDisjuncts(balancedFold)
          }), error = function(e){NA})
  
        if(!anyNA(overbalanced)){
          resultsSizes[algName, dataName] <- mean(unlist(overbalanced["numSmallDisjuncts", ]))
          resultsNums[algName, dataName] <- mean(unlist(overbalanced["meanCoverage", ]))
        }
      }
    }
  }
  list(sizes = resultsSizes, nums = resultsNums)
}
