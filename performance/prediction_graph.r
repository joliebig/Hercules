library(reshape2)
library(ggplot2)
draft = F
#if (draft) dev.off()
args<-commandArgs(TRUE)
csvData <- read.csv(file=args[1],head=FALSE, sep=",", stringsAsFactors=FALSE)

data <- as.numeric(csvData[1,])
deviations <- as.numeric(csvData[2,])

result_variant <- as.numeric(csvData[3,1])
result_simulator <- as.numeric(csvData[3,2])

max_range <- range(0, data, result_variant, result_simulator)

png(filename="prediction_graph.png")
plot(data, type="o", ylim=max_range, xlab="number of training models", ylab="time in ms", main="Performance Prediction")
abline(h=result_variant, col="blue", lty=8)
abline(h=result_simulator, col="red", lty=8)
dev.off()