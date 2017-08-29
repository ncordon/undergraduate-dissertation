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
# Load datasets from imbalance package and make partitions folds
######################################################################
datasets <- c("ecoli1", "glass0", "haberman", "iris0",
              "newthyroid1", "wisconsin","yeast4")

data(list = datasets)
numPartitions <- 3

dataFolds <- lapply(datasets, function(d){
  makePartition(eval(as.name(d)), numPartitions)
})
names(dataFolds) <- datasets
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

withoutNEATER <- getResults(filtering = FALSE)
withNEATER <- getResults(filtering = TRUE)

# xtable::xtable(t(withoutNEATER$nums))
# xtable::xtable(t(withoutNEATER$sizes))
# xtable::xtable(t(withNEATER$nums))
# xtable::xtable(t(withNEATER$sizes))

