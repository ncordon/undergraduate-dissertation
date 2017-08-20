library("RWeka")

# make an unpruned C4.5 tree out of dataset
arbol <- RWeka::J48(Class ~ ., ecoli1,
                     control = Weka_control(U = list(TRUE)))

leavesCoverage <- function(tree, classAttr){
  # retrieve tree in string format
  strgraph <- tree$classifier$graph()

  # select leaves corresponding to specified classAttr
  listmatches <- unlist(strsplit(strgraph, paste(classAttr, "\\(", sep = " ")))
  whichNumeric <- grep("^[[:digit:]]+", listmatches, perl = TRUE)
  listmatches <- listmatches[whichNumeric]

  # extract coverage number
  coverages <- lapply(listmatches, function(x){
    y <- sub(").*", "", x)
    sub("/.*", "", y)
  })
  as.numeric(unlist(coverages))
}

positiveCoverage <- leavesCoverage(arbol, classAttr = "positive")
negativeCoverage <- leavesCoverage(arbol, classAttr = "negative")
