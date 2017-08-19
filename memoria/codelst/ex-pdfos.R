library("imbalance")

data(newthyroid1)
set.seed(12345)

newSamples <- pdfos(newthyroid1, numInstances = 80)
# Join new samples with old imbalanced dataset
newDataset <- rbind(newthyroid1, newSamples)
# Plot a visual comparison between both datasets
plotComparison(newthyroid1, newDataset,
               attrs = names(newthyroid1)[1:3],
               cols = 2, classAttr = "Class")
