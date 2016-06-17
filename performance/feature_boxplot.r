library(reshape2)
library(ggplot2)
draft = F
#if (draft) dev.off()
args<-commandArgs(TRUE)

#options(width=180)
csvData <- read.csv(file=args[1],head=TRUE, sep=",", na.strings=c("","NA"), colClasses=c("time"="numeric"))

#solarize colors
solarizeColors <- c(
    rgb(133, 153,   0, maxColorValue = 255),  #{sol_green}  
    rgb( 38, 139, 210, maxColorValue = 255),  #{sol_blue}   
    rgb(181, 137,   0, maxColorValue = 255),  #{sol_yellow} 
    rgb(108, 113, 196, maxColorValue = 255),  #{sol_violet} 
    rgb(203,  75,  22, maxColorValue = 255),  #{sol_orange} 
    rgb(211,  54, 130, maxColorValue = 255),  #{sol_magenta}
    rgb(255, 255, 255, maxColorValue = 255),  #{white}
    rgb(220,  50,  47, maxColorValue = 255),  #{sol_red}    
    rgb( 42, 161, 152, maxColorValue = 255)  #{sol_cyan}  
    
)

# boxplots

#print (csvData)

#pdf(file=paste("feature_boxplots",".pdf",sep=""), width=7, height=5, onefile=TRUE, paper="special") 

#boxplot(csvData$time~csvData$feature, 
#    col=c(solarizeColors[1],solarizeColors[4],solarizeColors[9], solarizeColors[5]),
#    ylab="time", 
#    xlab="feature")

csvData.m <- melt(csvData,id.vars='feature', measure.vars='time')
#print(head(csvData.m))
p <- ggplot(csvData.m) + geom_boxplot(aes(x=feature, y=value),position = position_dodge(1)) + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=4))# + scale_x_discrete(label=abbreviate) # label=function(x) strtrim(x, 12) or label=abbreviate
ggsave("feature_boxplots.pdf", width=15, height=10)
warnings()
if (!draft) dev.off()
