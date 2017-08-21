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

source("./aux.R")

######################################################################
# Load datasets from imbalance package
######################################################################
datasets <- c("ecoli1", "glass0", "haberman", "iris0",
              "newthyroid1", "wisconsin","yeast4")

data(list = datasets)
numPartitions <- 3

######################################################################
# Prepare algorithms that will be used
######################################################################
algorithms <- c("none", "mwmote", "wracog", "rwo", "pdfos")
J48Wrapper <- structure(list(), class = "J48Wrapper")
trainWrapper.J48Wrapper <- function(wrapper, train, trainClass){
  train <- cbind(train, trainClass)
  colnames(train) <- colnames(train, do.NULL = FALSE)
  colnames(train)[length(colnames(train))] <- "Class"
  train <- data.frame(train)
  J48(Class ~ ., train)
}

# Prepare matrix for results
results <- matrix(ncol = length(datasets), nrow = length(algorithms))
colnames(results) <- datasets
rownames(results) <- algorithms
balancedSizes <- results
balancedNums <- results

# For each algorithm, for each dataset, get numPartitions executions and calc
# the mean size of coverages and the mean number of small disjuncts
for(algName in algorithms){
  if(algName != "none")
    algorithm <- eval(as.name(algName))
  
  for(dataName in datasets){
    overbalanced <- tryCatch(
      sapply(seq_along(dataFolds[[dataName]]), function(j){
        fold <- dataFolds[[dataName]][[j]]
        validation <- dataFolds[[dataName]][[j %% numPartitions + 1]]
        sizeMinority <- length(which(fold$Class == "positive"))
        sizeDataset <- nrow(fold)
        # We configure an imbalance ratio of 0.8
        numInstances <- ceiling(0.8 * (sizeDataset - sizeMinority) - sizeMinority)
        newSamples <- list()
        
        if(algName != "none"){
          if (algName != "wracog"){
            newSamples <- algorithm(fold, numInstances)
          } else{
            newSamples <- algorithm(fold, validation, wrapper = J48Wrapper)
          }
          balancedFold <- rbind(fold, newSamples)
        } else{
          balancedFold <- fold
        }
        infoSmallDisjuncts(balancedFold)
      }), error = function(e){NA})

    if(!anyNA(overbalanced)){
      balancedSizes[algName, dataName] <- mean(unlist(overbalanced["numSmallDisjuncts", ]))
      balancedNums[algName, dataName] <- mean(unlist(overbalanced["meanCoverage", ]))
    }
  }
}
