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
