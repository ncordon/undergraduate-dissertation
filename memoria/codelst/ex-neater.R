filteredSamples <- neater(newthyroid1, newSamples,
                          iterations = 500)
#> [1] "14 samples filtered by NEATER"
filteredNewDataset <- rbind(newthyroid1, filteredSamples)
plotComparison(newthyroid1, filteredNewDataset,
               attrs = names(newthyroid1)[1:3])
