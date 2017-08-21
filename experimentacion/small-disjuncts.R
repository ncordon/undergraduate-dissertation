pkgs <- c("devtools", "RWeka", "caret")

sapply(pkgs, function(pkg){
  if(!pkg %in% installed.packages())
    install.packages(pkg)
})

if(!"imbalance" %in% installed.packages())
  devtools::install_github("ncordon/imbalance")

library("RWeka")
library("caret")
library("imbalance")

######################################################################
# Load datasets from imbalance package
######################################################################
datasets <- c("ecoli1", "glass0", "haberman", "iris0",
              "newthyroid1", "wisconsin","yeast4")

data(list = datasets)
numPartitions <- 3
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


algorithms <- c("mwmote", "wracog", "rwo")
J48Wrapper <- structure(list(), class="J48Wrapper")
trainWrapper.J48Wrapper <- function(wrapper, train, trainClass){
  train <- cbind(train, trainClass)
  colnames(train) <- colnames(train, do.NULL = FALSE)
  colnames(train)[length(colnames(train))] <- "Class"
  train <- data.frame(train)
  J48(Class ~ ., train)
}


results <- matrix(ncol = length(datasets), nrow = length(algorithms))
colnames(results) <- datasets
rownames(results) <- algorithms
balancedSizes <- results
balancedNums <- results


for(dataName in datasets){
  for(algName in algorithms){
    algorithm <- eval(as.name(algName))

    overbalanced <- sapply(1:length(dataFolds[[dataName]]), function(j){
      fold <- dataFolds[[dataName]][[j]]
      validation <- dataFolds[[dataName]][[j %% numPartitions + 1]]
      sizeMinority <- length(which(fold$Class == "positive"))
      sizeDataset <- nrow(fold)
      # We configure an imbalance ratio of 0.8
      numInstances <- ceiling((sizeDataset - sizeMinority) * 0.8)

      if (algName != "wracog"){
        newSamples <- algorithm(fold, numInstances)
      } else{
        newSamples <- algorithm(fold, validation, wrapper = J48Wrapper)
      }
      balancedFold <- rbind(fold, newSamples)
      infoSmallDisjuncts(balancedFold)
    })

    balancedSizes[algName, dataName] <- mean(unlist(overbalanced["numSmallDisjuncts", ]))
    balancedNums[algName, dataName] <- mean(unlist(overbalanced["meanCoverage", ]))
  }
}
