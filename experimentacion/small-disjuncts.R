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

data(list=datasets)

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
  as.numeric(unlist(coverages))
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
  makePartition(eval(as.name(d)), numPartitions = 3 )
})
names(dataFolds) <- datasets


######################################################################
# make an unpruned C4.5 tree out of dataset
######################################################################
arbol <- RWeka::J48(Class ~ ., ecoli1,
                    control = Weka_control(U = list(TRUE), M = list(1)))

positiveCoverage <- leavesCoverage(arbol, classAttr = "positive")
negativeCoverage <- leavesCoverage(arbol, classAttr = "negative")
coverages <- c(positiveCoverage, negativeCoverage)
max(coverages)
